%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(cccp_callback_listener).

-behaviour(gen_listener).

-export([start_link/1
         ,handle_resource_response/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("cccp.hrl").

-record(state, {customer_number :: ne_binary()
                ,b_leg_number :: ne_binary()
                ,call = whapps_call:new() :: whapps_call:call()
                ,account_id :: ne_binary()
                ,account_cid :: ne_binary()
                ,queue :: api_binary()
                ,parked_call_id :: ne_binary()
                ,offnet_ctl_q :: ne_binary()
                ,auth_doc_id :: ne_binary()
                ,self = self() :: pid()
                ,relay_pid :: pid()
                ,callback_delay :: integer()
               }).

-type state() :: #state{}.

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_resource_response'},[{<<"*">>, <<"*">>}]}
                    ,{{'cccp_util', 'relay_amqp'}, [{<<"*">>, <<"*">>}]}
                    ]).

-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link([any()]) -> startlink_ret().
start_link(JObj) ->
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', ?BINDINGS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [JObj]).

-spec init(wh_json:object()) -> {'ok', state()}.
init([JObj]) ->
    CustomerNumber = wh_json:get_value(<<"Number">>, JObj),
    BLegNumber = wh_json:get_value(<<"B-Leg-Number">>, JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    OutboundCID = wh_json:get_value(<<"Outbound-Caller-ID-Number">>, JObj),
    AuthDocId = wh_json:get_value(<<"Auth-Doc-Id">>, JObj),
    CallbackDelay = wh_json:get_value(<<"Callback-Delay">>, JObj),
    DelayMs = case is_integer(CallbackDelay) of
                  'true' -> CallbackDelay * ?MILLISECONDS_IN_SECOND;
                  'false' ->
                      whapps_config:get_integer(?CCCP_CONFIG_CAT, <<"callback_delay">>, 3) * ?MILLISECONDS_IN_SECOND
              end,

    State = #state{customer_number = CustomerNumber
                   ,b_leg_number = BLegNumber
                   ,account_id = AccountId
                   ,account_cid = OutboundCID
                   ,auth_doc_id = AuthDocId
                   ,callback_delay = DelayMs
                  },

    {'ok', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'gen_listener', {'created_queue', Q}}, #state{queue = 'undefined'} = State) ->
    gen_listener:cast(self(), 'originate_park'),
    {'noreply', State#state{queue = Q}};
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    {'noreply', State};
handle_cast('originate_park', State) ->
    originate_park(State),
    {'noreply', State};
handle_cast({'call_update', CallUpdate}, State) ->
    {'noreply', State#state{call = CallUpdate}};
handle_cast({'relay_pid', PID}, State) when is_pid(PID) ->
    {'noreply', State#state{relay_pid = PID}};
handle_cast({'offnet_ctl_queue', CtrlQ}, State) ->
    {'noreply', State#state{offnet_ctl_q = CtrlQ}};
handle_cast({'originate_ready', JObj}, #state{call = Call
                                              ,customer_number = Number
                                             } = State) ->
    Q = wh_json:get_value(<<"Server-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Call0 = whapps_call:from_json(JObj, Call),
    NewCall = whapps_call:kvs_store('auth_cid', Number, Call0),
    Prop = [{<<"Call-ID">>, CallId}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_dialplan:publish_originate_execute(Q, Prop),
    cccp_platform_sup:new(NewCall),
    {'stop', 'normal', State};
handle_cast({'hangup_parked_call', _ErrMsg}, #state{parked_call_id = 'undefined'} = State) ->
    {'noreply', State};
handle_cast({'hangup_parked_call', _ErrMsg}, #state{parked_call_id = CallId
                                                    ,queue = Queue
                                                    ,offnet_ctl_q = ControllerQ
                                                   } = State) ->
    hangup_parked_call(CallId, Queue, ControllerQ),
    {'noreply', State#state{parked_call_id = 'undefined'}};
handle_cast('set_auth_doc_id', #state{auth_doc_id = AuthDocId
                                      ,call = Call
                                     } = State) ->
    CallUpdate = whapps_call:kvs_store('auth_doc_id', AuthDocId, Call),
    {'noreply', State#state{call = CallUpdate}};
handle_cast({'parked', CallId, ToDID}, State) ->
    _P = bridge_to_final_destination(CallId, ToDID, State),
    lager:debug("bridging to ~s (via ~s) in ~p", [ToDID, CallId, _P]),
    {'noreply', State#state{parked_call_id = CallId}};
handle_cast('stop_callback', State) ->
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{call = Call
                           ,relay_pid = PID
                           ,b_leg_number = Number
                          }) ->
    {'reply', [{'call', Call}
               ,{'relay_pid', PID}
               ,{'b_leg_number', Number}
              ]}.

-spec handle_resource_response(wh_json:object(), wh_proplist()) -> 'ok'.
handle_resource_response(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case wh_util:get_event_type(JObj) of
        {<<"resource">>, <<"offnet_resp">>} ->
            ResResp = wh_json:get_value(<<"Resource-Response">>, JObj),
            handle_originate_ready(ResResp, Props);
        {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
            gen_listener:cast(Srv, 'stop_callback');
        {<<"call_event">>,<<"CHANNEL_EXECUTE_COMPLETE">>} ->
            cccp_util:handle_disconnect(JObj, Props);
        {<<"resource">>,<<"originate_resp">>} ->
            handle_originate_response(JObj, Srv);
        {<<"error">>,<<"originate_resp">>} ->
            gen_listener:cast(Srv, {'hangup_parked_call', wh_json:get_value(<<"Error-Message">>, JObj)});
        _ -> 'ok'
    end.

-spec handle_originate_response(wh_json:object(), server_ref()) -> 'ok'.
handle_originate_response(JObj, Srv) ->
    case {wh_json:get_value(<<"Application-Name">>, JObj)
          ,wh_json:get_value(<<"Application-Response">>, JObj)
         }
    of
        {<<"bridge">>, <<"SUCCESS">>} ->
            gen_listener:cast(Srv, 'stop_callback');
        {<<"park">>, <<"SUCCESS">>} ->
            gen_listener:cast(Srv, 'set_auth_doc_id');
        _ -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec originate_park(state()) -> 'ok'.
originate_park(#state{callback_delay = Delay} = State) ->
    _ = timer:sleep(Delay),
    wapi_offnet_resource:publish_req(create_request(State)).

-spec create_request(state()) -> wh_proplist().
create_request(#state{account_id = AccountId
                      ,customer_number = Number
                      ,account_cid = CID
                      ,queue = Queue
                     }) ->
    CCVs = [{<<"Account-ID">>, AccountId}],
    [{<<"Application-Name">>, <<"park">>}
     ,{<<"Resource-Type">>, <<"originate">>}
     ,{<<"Originate-Immediate">>, 'true'}
     ,{<<"To-DID">>, Number}
     ,{<<"Outbound-Caller-ID-Number">>, CID}
     ,{<<"Progress-Timeout">>, 12}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
     ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>]}
     | wh_api:default_headers(Queue, ?APP_NAME, ?APP_VERSION)
    ].

-spec handle_originate_ready(wh_json:object(), proplist()) -> 'ok'.
handle_originate_ready(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case wh_util:get_event_type(JObj) of
        {<<"dialplan">>, <<"originate_ready">>} ->
            gen_listener:cast(Srv, {'originate_ready', JObj});
        _ -> 'ok'
    end.

-spec hangup_parked_call(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
hangup_parked_call(ParkedCallId, Q, CtrlQ) ->
    Hangup = [{<<"Application-Name">>, <<"hangup">>}
              ,{<<"Insert-At">>, <<"now">>}
              ,{<<"Call-ID">>, ParkedCallId}
              | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
             ],
    wapi_dialplan:publish_command(CtrlQ, props:filter_undefined(Hangup)).

-spec bridge_to_final_destination(ne_binary(), ne_binary(), state()) -> 'ok'.
bridge_to_final_destination(CallId, ToDID, #state{customer_number = Number
                                                  ,queue = Queue
                                                  ,offnet_ctl_q = ControllerQ
                                                  ,account_id = AccountId
                                                  ,account_cid = CID
                                                  ,auth_doc_id = AuthDocId
                                                 }) ->
    cccp_util:bridge(CallId, ToDID, Number, Queue, ControllerQ, AccountId, CID),

    case is_binary(AuthDocId) of
        'false' -> 'ok';
        'true' -> cccp_util:store_last_dialed(ToDID, AuthDocId)
    end.
