%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(cccp_platform_listener).

-behaviour(gen_listener).

-export([start_link/1
         ,send_route_win/2
         ,handle_answer/2
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

-record(state, {call = whapps_call:new() :: whapps_call:call()
                ,flow = wh_json:new() :: wh_json:object()
                ,relay_pid :: api_pid()
                ,queue :: api_binary()
                ,self = self() :: pid()
               }).

-define(MAX_ATTEMPTS, whapps_config:get(?CCCP_CONFIG_CAT, <<"tries_count">>, 3)).
-define(PLATFORM_COLLECT_TIMEOUT, whapps_config:get_integer(?CCCP_CONFIG_CAT, <<"platform_collect_timeout">>, 5000)).
-define(PLATFORM_INTERDIGIT_TIMEOUT, whapps_config:get_integer(?CCCP_CONFIG_CAT, <<"platform_interdigit_timeout">>, 5000)).
-define(PLATFORM_LONGPIN_LENGTH, whapps_config:get_integer(?CCCP_CONFIG_CAT, <<"platform_long_pin_length">>, 10)).
-define(PLATFORM_SHORTPIN_LENGTH, whapps_config:get_integer(?CCCP_CONFIG_CAT, <<"platform_short_pin_length">>, 4)).
-define(PLATFORM_SHORTPIN_TIMEOUT, whapps_config:get_integer(?CCCP_CONFIG_CAT, <<"platform_short_pin_timeout">>, 5000)).
-define(PLATFORM_ORIGINATOR, whapps_config:get_binary(?CCCP_CONFIG_CAT, <<"platform_origiantor_type">>, <<"CCCP">>)).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(CallId), [{'self', []}
                           ,{'call', [{'callid', CallId}]}
                          ]).

-define(RESPONDERS, [{{'cccp_util', 'relay_amqp'},[{<<"call_event">>, <<"*">>}]}
                    ,{{'cccp_util', 'handle_disconnect'},[{<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>}]}
                    ,{{?MODULE, 'handle_answer'}, [{<<"call_event">>, <<"*">>}]}
                    ,{{?MODULE, 'send_route_win'} ,[{<<"dialplan">>, <<"route_resp">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================

-spec send_route_win(wh_json:object(), wh_proplist()) -> 'ok'.
send_route_win(JObj, Props) ->
    'true' = wapi_route:resp_v(JObj) andalso wh_json:get_value(<<"App-Name">>, JObj) =:= <<"callflow">>,
    Win = props:get_value('route_win', Props),
    lager:info("Send route win!"),
    wapi_route:publish_win(wh_json:get_value(<<"Server-ID">>, JObj), Win).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Call) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS(whapps_call:call_id(Call))}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ], [Call]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Call]) ->
    CallUpdate = whapps_call:kvs_store('server_pid', self(), Call),
    {'ok', #state{call=CallUpdate}}.

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
handle_cast({'gen_listener',{'created_queue',Queue}}, #state{call=Call}=State) ->
    {'noreply', State#state{call=whapps_call:set_controller_queue(Queue, Call)}};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{call=Call}=State) ->
    whapps_call_command:answer(Call),
    {'noreply', State};
handle_cast({'relay_pid', PID}, State) when is_pid(PID)->
    {'noreply', State#state{relay_pid = PID}};
handle_cast('stop_platform_listener', State) ->
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:info("Unhandled msg: ~p", [_Msg]),
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
handle_event(_JObj, #state{relay_pid = Pid
                           ,call = Call
                          }) ->
    Props = case kz_call_event:event_name(_JObj) of
                <<"route_resp">> -> [{'route_win', [{<<"Msg-ID">>, whapps_call:call_id(Call)}
                                                    ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                                                    ,{<<"Control-Queue">>, whapps_call:control_queue(Call)}
                                                    ,{<<"Custom-Channel-Vars">>, whapps_call:custom_channel_vars(Call)}
                                                    | wh_api:default_headers(whapps_call:controller_queue(Call)
                                                                             ,<<"dialplan">>
                                                                             ,<<"route_win">>
                                                                             ,?APP_NAME
                                                                             ,?APP_VERSION
                                                                            )
                                                    ]}];
                _ -> []
    end,
    {'reply', [{'relay_pid', Pid}
               ,{'call', Call}
               | Props
              ]}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

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

handle_answer(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>,<<"CHANNEL_ANSWER">>} ->
            gen_listener:cast(Srv, {'relay_pid', self()}),
            process_call_to_platform(props:get_value('call', Props));
        {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
            gen_listener:cast(Srv, 'stop_platform_listener');
        _ -> 'ok'
    end.

-spec process_call_to_platform(whapps_call:call()) -> 'ok'.
process_call_to_platform(Call) ->
    wh_util:put_callid(whapps_call:call_id(Call)),
    CID = cccp_util:caller_cid(Call),
    case (not cccp_blocking:is_cid_blocked(CID)) andalso authorize(Call) of
        'false' ->
            lager:info("CID blocked! Hangup."),
            whapps_call_command:b_prompt(cccp_util:retries_exceeded(), Call),
            whapps_call_command:hangup(Call);
        'fail' ->
            lager:info("Authorization failed! Hangup."),
            whapps_call_command:hangup(Call);
        Auth ->
            lager:info("Authorized! Relaying call..."),
            relay_call(Auth, Call)
    end.

-spec authorize(whapps_call:call()) -> cccp_auth:cccp_auth() | 'fail'.
-spec authorize(whapps_call:call(), cccp_auth:cccp_auth_ret()) -> cccp_auth:cccp_auth() | 'fail'.
authorize(Call) ->
    CID = cccp_util:caller_cid(Call),
    Auth = cccp_auth:authorize([CID, <<"*">>], cccp_util:pin_listing()),
    authorize(Call, Auth).
authorize(Call, {'error', 'empty'}) ->
    CID = cccp_util:caller_cid(Call),
    ViewOptions = [{'key', CID}],
    PinType = case couch_mgr:get_results(?KZ_CCCPS_DB, cccp_util:cid_listing(), ViewOptions) of
                  {'ok', []} -> 'long_pin';
                  {'ok', _} -> 'short_pin';
                  {'error', _Err} ->
                      lager:error("can't lookup auth by cid ~s, use long pin", [CID]),
                      'long_pin'
              end,
    pin_auth(Call, PinType);
authorize(Call, {'error', _Err}) ->
    lager:error("Can't authorize coz of ~p", [_Err]),
    pin_auth(Call, 'long_pin');
authorize(_Call, {'ok', Auth}) ->
    Auth.

-type pin_type() :: 'long_pin' | 'short_pin'.
-spec pin_auth(whapps_call:call(), pin_type()) -> cccp_auth:cccp_auth() | 'fail' | 'match'.
-spec pin_auth(whapps_call:call(), pin_type(), term(), integer()) -> cccp_auth:cccp_auth() | 'fail' | 'match'.
pin_auth(Call, Pin) ->
    pin_auth(Call, Pin, 'collect', ?MAX_ATTEMPTS).
pin_auth(Call, _Pin, _State, Attempts) when Attempts =< 0 ->
    cccp_blocking:block_cid(cccp_util:caller_cid(Call)),
    whapps_call_command:b_prompt(cccp_util:retries_exceeded(), Call),
    'fail';
pin_auth(Call, PinType, 'collect', Attempts) ->
    PinPrompt = case PinType of
                    'long_pin' -> cccp_util:request_long_pin();
                    'short_pin' -> cccp_util:request_short_pin()
                end,
    case pin_collect(PinPrompt, Call) of
        {'ok', EnteredPin} ->
            lager:debug("Checking ~p", [EnteredPin]),
            pin_auth(Call, PinType, {'check', EnteredPin}, Attempts);
        {'error', Err} ->
            lager:error("Can't collect pin: ~p", [Err]),
            pin_auth(Call, PinType, 'collect', Attempts - 1)
    end;
pin_auth(Call, 'short_pin' = PinType, {'check', Pin}, Attempts) when is_binary(Pin) ->
    lager:info("Trying to authorize by pin ~s", [Pin]),
    CID = cccp_util:caller_cid(Call),
    case cccp_auth:authorize([CID, Pin], cccp_util:pin_listing()) of
        {'ok', Auth} -> Auth;
        _ ->
            case cccp_auth:authorize([<<"*">>, Pin], cccp_util:pin_listing()) of
                {'ok', Auth} -> Auth;
                _ ->
                    whapps_call_command:b_prompt(cccp_util:invalid_pin(), Call),
                    pin_auth(Call, PinType, 'collect', Attempts - 1)
            end
    end;
pin_auth(Call, 'long_pin' = PinType, {'check', Pin}, Attempts) when is_binary(Pin) ->
    lager:info("Trying to authorize by pin ~s", [Pin]),
    case cccp_auth:authorize([<<"*">>, Pin], cccp_util:pin_listing()) of
        {'ok', Auth} -> Auth;
        _ ->
            whapps_call_command:b_prompt(cccp_util:invalid_pin(), Call),
            pin_auth(Call, PinType, 'collect', Attempts - 1)
    end.

-spec pin_collect(ne_binary(), whapps_call:call()) -> whapps_call_command:b_play_and_collect_digits_return().
pin_collect(PinPrompt, Call) ->
    whapps_call_command:b_prompt_and_collect_digits(4
                                                    ,12
                                                    ,PinPrompt
                                                    ,1
                                                    ,?PLATFORM_COLLECT_TIMEOUT
                                                    ,?PLATFORM_INTERDIGIT_TIMEOUT
                                                    ,?PLATFORM_LONGPIN_LENGTH
                                                    ,?PLATFORM_SHORTPIN_LENGTH
                                                    ,?PLATFORM_SHORTPIN_TIMEOUT
                                                    ,Call
                                                   ).

-spec relay_call(cccp_auth:cccp_auth(), whapps_call:call()) -> 'ok'.
relay_call(Auth, Call) ->
    RequestUser = whapps_config:get(?CCCP_CONFIG_CAT, <<"callflow_number">>, <<"cccp_handler">>),
    NewRequest = iolist_to_binary([RequestUser, <<"@">>, whapps_call:request_realm(Call)]),
    NewCall = whapps_call:exec([{fun whapps_call:set_account_id/2, cccp_auth:account_id(Auth)}
                                ,{fun whapps_call:set_request/2, NewRequest}
                                ,{fun whapps_call:set_custom_channel_var/3, <<"Originator-Type">>, ?PLATFORM_ORIGINATOR}
                               ], Call),
    whapps_call_command:set('undefined'
                            ,wh_json:set_value(<<"Originator-Type">>, ?PLATFORM_ORIGINATOR, wh_json:new())
                            ,NewCall
                           ),
    RouteReq = from_call(NewCall),
    lager:info("Publishing route req"),
    wh_amqp_worker:cast(RouteReq, fun wapi_route:publish_req/1).

-spec from_call(whapps_call:call()) -> wh_proplist().
from_call(Call) ->
    [{<<"Msg-ID">>, whapps_call:fetch_id(Call)}
     ,{<<"Call-ID">>, whapps_call:call_id(Call)}
     ,{<<"Message-ID">>, wh_util:rand_hex_binary(6)}
     ,{<<"Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
     ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
     ,{<<"To">>, whapps_call:to(Call)}
     ,{<<"From">>, whapps_call:from(Call)}
     ,{<<"Request">>, whapps_call:request(Call)}
     ,{<<"Switch-Nodename">>, whapps_call:switch_nodename(Call)}
     ,{<<"Switch-Hostname">>, whapps_call:switch_hostname(Call)}
     ,{<<"Switch-URL">>, whapps_call:switch_url(Call)}
     ,{<<"Switch-URI">>, whapps_call:switch_uri(Call)}
     ,{<<"Custom-Channel-Vars">>, whapps_call:custom_channel_vars(Call)}
     ,{<<"Custom-SIP-Headers">>, whapps_call:custom_sip_headers(Call)}
     ,{<<"Resource-Type">>, whapps_call:resource_type(Call)}
     ,{<<"To-Tag">>, whapps_call:to_tag(Call)}
     ,{<<"From-Tag">>, whapps_call:from_tag(Call)}
     | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
    ].
