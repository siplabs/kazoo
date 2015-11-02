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

-record(state, {call_id
                ,endpoint
                ,endpoints
                ,control_q
                ,reply_to
                ,call_vars = []
                ,channel_vars = []
                ,self = self()
               }).

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
    CallId = <<"loopback-", (couch_mgr:get_uuid())/binary>>,
    Bindings = [{'call', [{'callid', CallId}]}
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
    wh_util:put_callid(CallId),
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
handle_call({api, _App, _Data}, _From, State) ->
    lager:debug([{trace, true}], "api call: application ~s, data ~p", [_App, _Data]),
    {reply, {ok, <<"loopback">>}, State};
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
handle_cast({gen_listener, {created_queue, CtrlQ}}, #state{call_id = CallId
                                                           ,endpoint = E
                                                           ,call_vars = CallVars
                                                           ,channel_vars = CCVs
                                                          } = State) ->
    NewState = State#state{control_q = CtrlQ},
    EndpointCCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, E),
    Realm = wh_json:get_value(<<"To-Realm">>, E),
    Number = wh_json:get_value(<<"Route">>, E),
    ResourceType = <<"audio">>,
%    ResourceType = wh_json:get_value(<<"Resource-Type">>, JObj, <<"audio">>),
    RouteReq = [{<<"From">>, <<"unknown@unknown">>}
           ,{<<"To">>, <<"unknown@unknown">>}
           ,{<<"Request">>, <<Number/binary, "@", Realm/binary>>}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Caller-ID-Number">>, <<"0">>}
           ,{<<"Caller-ID-Name">>, <<"Loopback">>}
           ,{<<"Resource-Type">>, ResourceType}
           ,{<<"Custom-Channel-Vars">>, EndpointCCVs}
           | wh_api:default_headers(<<"dialplan">>, <<"route_req">>, ?APP_NAME, ?APP_VERSION)
          ],
    {ok, Resp} = wh_amqp_worker:call(RouteReq
                                     ,fun wapi_route:publish_req/1
                                     ,fun wapi_route:is_actionable_resp/1
                                     ,2000),
    RespCCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, Resp, wh_json:new()),
    RespCallVars = wh_json:get_value(<<"Custom-Call-Vars">>, Resp, wh_json:new()),
    lager:debug([{trace, true}], "resp ~p", [Resp]),
    ServerId = wh_json:get_value(<<"Server-ID">>, Resp),
    RouteWin = [{<<"Call-ID">>, CallId}
              ,{<<"Custom-Channel-Vars">>, CCVs}
              ,{<<"Server-ID">>, ServerId}
              ,{<<"Control-Queue">>, CtrlQ}
              | wh_api:default_headers(<<"dialplan">>, <<"route_win">>
                                       ,?APP_NAME, ?APP_VERSION)],
    wh_amqp_worker:cast(RouteWin
                        ,fun(Payload) -> wapi_route:publish_win(ServerId, Payload) end
                       ),
    lager:debug([{trace, true}], "published route win"),
    {noreply, NewState#state{channel_vars = [RespCCVs, EndpointCCVs | CCVs]
                             ,call_vars = [RespCallVars | CallVars]
                            }};
handle_cast({call_command, Cmd}, State) ->
    Application = wh_json:get_value(<<"Application-Name">>, Cmd),
    handle_command(Application, Cmd, State);
handle_cast({gen_listener,{is_consuming,true}}, State) ->
    {noreply, State};
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @spec handle_event(Event, State) -> {reply, proplist()}
%% @end
%%--------------------------------------------------------------------
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
handle_command(<<"set">>, Data, #state{channel_vars = ChannelVars, call_vars = CallVars} = State) ->
    DataCallVars = wh_json:get_value(<<"Custom-Call-Vars">>, Data, wh_json:new()),
    DataChannelVars = wh_json:get_value(<<"Custom-Channel-Vars">>, Data, wh_json:new()),
    {noreply, State#state{channel_vars = [DataChannelVars | ChannelVars]
                          ,call_vars = [DataCallVars | CallVars]}};
handle_command(<<"bridge">>, Data, State) ->
    Endpoints = wh_json:get_value(<<"Endpoints">>, Data, []),
    NewState = State#state{endpoints = Endpoints},
    maybe_reply(NewState);
handle_command(Application, Data, #state{call_id = CallId} = State) ->
    lager:debug([{trace, true}], "unknown application ~p", [Application]),
    lager:debug([{trace, true}], "data ~p", [Data]),
    fs_cmd(CallId, Data),
    {noreply, State}.

maybe_reply(#state{reply_to = undefined} = State) ->
    lager:debug("reply to is undefined"),
    {noreply, State};
maybe_reply(#state{endpoints = undefined} = State) ->
    lager:debug("endpoints are undefined"),
    {noreply, State};
maybe_reply(#state{reply_to = ReplyTo
                   ,endpoints = Endpoints
                   ,call_vars = CallVars
                   ,channel_vars = ChannelVars} = State) ->
    lager:debug("call vars ~p", [CallVars]),
    lager:debug("channel vars ~p", [ChannelVars]),
    gen_server:reply(ReplyTo, {ok, Endpoints}),
    {noreply, State}.

fs_cmd(CallId, Data) ->
    spawn(send_fs_cmd(CallId, Data)).

send_fs_cmd(CallId, Data) ->
    wh_util:put_callid(CallId),
    Self = self(),
    fun() ->
            ecallmgr_call_command:exec_cmd(Self, CallId, Data, Self)
    end.
