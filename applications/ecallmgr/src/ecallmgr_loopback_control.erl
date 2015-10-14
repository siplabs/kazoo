-module(ecallmgr_loopback_control).

-behaviour(gen_server).

%% API functions
-export([start_link/1
         ,get_endpoints/1
         ,handle_call_command/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_event/2,
         terminate/2,
         code_change/3]).

-define(RESPONDERS, [{{?MODULE, 'handle_call_command'}
                      ,[{<<"call">>, <<"command">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {call_id, endpoint, endpoints, control_q, reply_to, self = self()}).

-include("ecallmgr.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Endpoint) ->
    CallId = <<<<"loopback-">>/binary, (couch_mgr:get_uuid())/binary>>,
    Bindings = [{'call', [{'callid', CallId}
                          ,{'restrict_to', [<<"usurp_control">>, <<"hangup_call">>]}
                         ]}
                ,{'dialplan', []}
                ,{'self', []}
               ],
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', Bindings}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ]
                            ,[CallId, Endpoint]).

get_endpoints(Pid) ->
    gen_server:call(Pid, get_endpoints).

handle_call_command(JObj, Props) ->
    Pid = props:get_value(self, Props),
    lager:debug("sending command to ~p", [Pid]),
    lager:debug("command ~p", [JObj]),
    gen_server:cast(Pid, {call_command, JObj}).

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
init([CallId, Endpoint]) ->
    {ok, #state{call_id = CallId, endpoint = Endpoint}}.

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
handle_call(get_endpoints, From, State) ->
    lager:debug([{trace, true}], "get endpoints"),
    maybe_reply(State#state{reply_to = From});
handle_call(_Request, _From, State) ->
    lager:debug([{trace, true}], "unknown call ~p", [_Request]),
    {noreply, State}.

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
handle_cast({gen_listener, {created_queue, CtrlQ}}, #state{call_id = CallId, endpoint = E} = State) ->
    NewState = State#state{control_q = CtrlQ},
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, E),
    Realm = wh_json:get_value(<<"To-Realm">>, E),
    Number = wh_json:get_value(<<"Route">>, E),
    ResourceType = <<"audio">>,
%    ResourceType = wh_json:get_value(<<"Resource-Type">>, JObj, <<"audio">>),
    Req = [{<<"From">>, <<"unknown@unknown">>}
           ,{<<"To">>, <<"unknown@unknown">>}
           ,{<<"Request">>, <<Number/binary, "@", Realm/binary>>}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Caller-ID-Number">>, <<"0">>}
           ,{<<"Caller-ID-Name">>, <<"Loopback">>}
           ,{<<"Resource-Type">>, ResourceType}
           ,{<<"Custom-Channel-Vars">>, CCVs}
           | wh_api:default_headers(<<"dialplan">>, <<"route_req">>, ?APP_NAME, ?APP_VERSION)
          ],
    {ok, Resp} = wh_amqp_worker:call(Req
                                     ,fun wapi_route:publish_req/1
                                     ,fun wapi_route:is_actionable_resp/1
                                     ,2000),
    lager:debug([{trace, true}], "resp ~p", [Resp]),
    ServerId = wh_json:get_value(<<"Server-ID">>, Resp),
    ReqWin = [{<<"Call-ID">>, CallId}
              ,{<<"Custom-Channel-Vars">>, CCVs}
              ,{<<"Server-ID">>, ServerId}
              ,{<<"Control-Queue">>, CtrlQ}
              | wh_api:default_headers(<<"dialplan">>, <<"route_win">>
                                       ,?APP_NAME, ?APP_VERSION)],
    wh_amqp_worker:cast(ReqWin
                        ,fun(Payload) -> wapi_route:publish_win(ServerId, Payload) end
                       ),
    {noreply, NewState};
handle_cast({call_command, Cmd}, State) ->
    Application = wh_json:get_value(<<"Application-Name">>, Cmd),
    handle_command(Application, Cmd, State);
handle_cast(_Msg, State) ->
    lager:debug([{trace, true}], "unknown cast ~p", [_Msg]),
    {noreply, State}.

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
    lager:debug([{trace, true}], "unknown info ~p", [_Info]),
    {noreply, State}.

handle_event(_Event, #state{self = Self} = _State) ->
    {'reply', [{self, Self}]}.

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
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%handle_command(<<"set">>, _Data, State) ->
%    {noreply, State};
handle_command(<<"bridge">>, Data, State) ->
    Endpoints = wh_json:get_value(<<"Endpoints">>, Data, []),
    NewState = State#state{endpoints = Endpoints},
    maybe_reply(NewState);
handle_command(_Application, _Data, State) ->
    lager:debug([{trace, true}], "unknown application ~p", [_Application]),
    {noreply, State}.

maybe_reply(#state{reply_to = undefined} = State) ->
    lager:debug("reply to is undefined"),
    {noreply, State};
maybe_reply(#state{endpoints = undefined} = State) ->
    lager:debug("endpoints are undefined"),
    {noreply, State};
maybe_reply(#state{reply_to = ReplyTo, endpoints = Endpoints} = State) ->
    gen_server:reply(ReplyTo, {ok, Endpoints}),
    {noreply, State}.
