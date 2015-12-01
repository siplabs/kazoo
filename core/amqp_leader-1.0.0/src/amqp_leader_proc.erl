-module(amqp_leader_proc).

-behaviour(gen_server).

-compile({no_auto_import,[node/1]}).

%% API functions
-export([start_link/6
         ,leader_call/2
         ,call/2
         ,alive/1
         ,broadcast/3
        ]).
-export([s/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name
                ,leader
                ,role
                ,elected = 0
                ,restarted = 0
                ,callback_module
                ,callback_state
                ,down = []
                ,candidates = [node()]
               }).

-record(sign, {elected
               ,restarted
               ,node = node()
               ,name
               ,sync
               ,candidates
              }).

-record(?MODULE, {from, msg}).

-include("amqp_leader.hrl").

-define(is_leader, State#state.role =:= 'leader').
-define(from_leader, (State#state.role =/= 'leader')
        andalso
        (From#sign.node =:= State#state.leader#sign.node
         andalso From#sign.elected =:= State#state.leader#sign.elected
         andalso From#sign.restarted =:= State#state.leader#sign.restarted
        )
       ).
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
start_link(Name, CandidateNodes, OptArgs, Mod, Arg, Options) ->
    gen_server:start_link({'local', Name}, ?MODULE, [self(), Name, CandidateNodes, OptArgs, Mod, Arg, Options], []).

leader_call(Name, Request) ->
    gen_server:call(Name, {'leader_call', Request}, 20000).

alive(#sign{candidates = Candidates}) ->
    Candidates.

call(Name, Request) ->
    gen_server:call(Name, Request, 20000).

broadcast(Msg, _Nodes, #sign{name = Name}) ->
    send({Name, 'broadcast'}, Msg).

s(Name) ->
    gen_server:call(Name, s).

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
init([Starter, Name, _CandidateNodes, _OptArgs, Mod, Arg, _Options]) ->
    proc_lib:init_ack(Starter, {'ok', self()}),
    wh_util:put_callid(wapi_leader:queue()),
    true = amqp_leader_listener:is_ready(),
    case catch Mod:init(Arg) of
        {'stop', _Reason} = Stop -> Stop;
        {'ignore', _, _} -> 'ignore';
        {'EXIT', Reason} -> {'stop', Reason};
        {'ok', ModState} ->
            State = #state{name = Name
                           ,callback_module = Mod
                           ,callback_state = ModState},
            send({Name, 'broadcast'}, {?MODULE, sign(State), 'join'}),
            receive
                #?MODULE{msg = {'leader', Leader}, from = Leader} ->
                    Routines = [{fun set_leader/2, Leader}
                                ,{fun set_role/2, 'candidate'}
                                ,{fun call_surrendered/2, Leader}
                                ,{fun announce_leader/2, {Leader, 'me'}}
                                ,{fun add_candidates/2, Leader}
                               ],
                    ok(State, Routines)
            after
                3000 ->
                    Routines = [{fun set_leader/2, 'me'}
                                ,{fun set_role/2, 'leader'}
                                ,{fun call_elected/2, 'undefined'}
                                ,{fun announce_leader/2, {'broadcast', 'me'}}
                               ],
                    ok(State, Routines)
            end;
        Else ->
            lager:warning("~s initialization bad return ~p", [Mod, Else]),
            {'stop', {'bad_return_value', Else}}
    end.

ok(State, []) ->
    {'ok', State};
ok(State, [{Fun, Data} | Rest]) ->
    case Fun(State, Data) of
        #state{} = NewState -> ok(NewState, Rest);
        _ -> ok(State, Rest)
    end.
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
handle_call(s, _, State) ->
    {reply, State, State};
handle_call({'leader_call', Msg}, From, State) when ?is_leader ->
    Routines = [{fun call_handle_leader_call/2, {From, Msg}}
               ],
    noreply(State, Routines);
handle_call({'leader_call', Msg}, From, #state{name = Name} = State) ->
    send(leader(State), {'leader_call', {{Name, node()}, From}, Msg}),
    noreply(State, []);
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'unhandled'}, State}.

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
handle_info(#?MODULE{from = From, msg = 'join'} = Msg, State) when ?is_leader ->
    lager:debug("message ~p", [Msg]),
    Routines = [{fun call_elected/2, From}
                ,{fun announce_leader/2, {From, 'me'}}
                ,{fun add_candidates/2, From}
               ],
    noreply(State, Routines);

handle_info(#?MODULE{from = From, msg = 'join'} = Msg, State) ->
    lager:debug("message ~p", [Msg]),
    Routines = [{fun add_candidates/2, From}
               ],
    noreply(State, Routines);

handle_info(#?MODULE{msg = {'leader', Me}} = Msg, #state{leader = Me} = State) when ?is_leader ->
    lager:debug("message ~p", [Msg]),
    Routines = [{fun increase_elected/2, []}
                ,{fun set_leader/2, 'me'}
                ,{fun announce_leader/2, {'broadcast', sign(State)}}
               ],
    noreply(State, Routines);

handle_info(#?MODULE{msg = {'leader', NotMe}} = Msg, State) when ?is_leader ->
    lager:debug("message ~p", [Msg]),
    case NotMe > sign(State) of
        'true' ->
            Routines = [{fun set_leader/2, NotMe}
                        ,{fun set_role/2, 'candidate'}
                        ,{fun announce_leader/2, {'broadcast', 'me'}}
                        ,{fun surrender/2, NotMe}
                        ,{fun call_surrendered/2, leader(State)}
                       ],
            noreply(State, Routines);
        'false' ->
            Routines = [{fun announce_leader/2, {NotMe, 'me'}}
                       ],
            noreply(State, Routines)
    end;

handle_info(#?MODULE{from = From, msg = {'leader', NewLeader}} = Msg, State) when ?from_leader ->
    lager:debug("message ~p", [Msg]),
    Routines = [{fun set_leader/2, NewLeader}
                ,{fun call_surrendered/2, leader(State)}
               ],
    noreply(State, Routines);

handle_info(#?MODULE{from = From, msg = Msg}, State) when ?from_leader ->
    Routines = [{fun call_from_leader/2, Msg}
               ],
    noreply(State, Routines);

handle_info({'leader_call', From, Request}, State) when ?is_leader ->
    Routines = [{fun call_handle_leader_call/2, {From, Request}}
               ],
    noreply(State, Routines);

handle_info({{Pid, _} = From, Reply}, State) when is_pid(Pid) andalso erlang:node(Pid) =:= node() ->
    gen_server:reply(From, Reply),
    noreply(State, []);

handle_info({'DOWN', Node} = Msg, #state{leader = Leader} = State) ->
    lager:debug("message ~p", [Msg]),
    case node(Leader) of
        Node ->
            Routines = [{fun increase_elected/2, []}
                        ,{fun set_leader/2, 'me'}
                        ,{fun set_role/2, 'leader'}
                        ,{fun call_elected/2, 'undefined'}
                        ,{fun announce_leader/2, {'broadcast', Leader}}
                       ],
            noreply(State, Routines);
        _ ->
            Routines = [],
            noreply(State, Routines)
    end;

handle_info(_Info, State) ->
    lager:error("unhandled msg ~p", [_Info]),
    lager:error("state ~p", [State]),
    noreply(State, []).

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
surrender(State, To) ->
    send({State, 'broadcast'}, #?MODULE{from = sign(State), msg = {'leader', To}}),
    set_role(State, 'candidate').

call_surrendered(#state{leader = Leader} = State, Leader) ->
    State;
call_surrendered(#state{callback_module = Mod} = State, 'undefined') ->
    LeaderSync = leader_sync(State),
    {'ok', NewModState} = Mod:surrendered(callback_state(State), LeaderSync, State),
    set_callback_state(State, NewModState);
call_surrendered(State, OldLeader) ->
    LeaderSync = leader_sync(State),
    case sign_sync(OldLeader) =:= LeaderSync of
        'true' ->
            State;
        'false' ->
            call_surrendered(State, 'undefined')
    end.

call_elected(State, #sign{} = From) ->
    call_elected(State, node(From));
call_elected(#state{callback_module = Mod, leader = Leader} = State, Node) when ?is_leader ->
    {_Action, Sync, ModState} = Mod:elected(callback_state(State), leader(State), Node),
    set_callback_state(State#state{leader = Leader#sign{sync = Sync}}, ModState).

call_handle_leader_call(#state{callback_module = Mod} = State, {From, Msg}) when ?is_leader ->
    case Mod:handle_leader_call(Msg, From, callback_state(State), leader(State)) of
        {'reply', Reply, Broadcast, NewModState} ->
            reply(From, Reply),
            send({name(State), 'broadcast'}, #?MODULE{from = leader(State), msg = Broadcast}),
            {set_callback_state(State, NewModState), [{fun announce_leader/2, {'broadcast', 'me'}}]};
        {'reply', Reply, NewModState} ->
            reply(From, Reply),
            set_callback_state(State, NewModState);
        {'stop', Reason, Reply, NewModState} ->
            reply(From, Reply),
            {set_callback_state(State, NewModState), {'stop', Reason}};
        {'noreply', NewModState} ->
            set_callback_state(State, NewModState);
        {'stop', Reason, NewModState} ->
            {set_callback_state(State, NewModState), {'stop', Reason}}
    end.

call_from_leader(#state{callback_module = Mod} = State, Sync) ->
    case Mod:from_leader(Sync, callback_state(State), leader(State)) of
        {'ok', NewModState} ->
            set_callback_state(State, NewModState);
        {'noreply', NewModState} ->
            set_callback_state(State, NewModState);
        {'stop', Reason, NewModState} ->
            {set_callback_state(State, NewModState), {'stop', Reason}}
    end.

%handle_callback_return({'ok'

noreply(#state{} = State, []) ->
    {'noreply', State};
noreply(#state{} = State, [{'stop', Reason} | _]) ->
    {'stop', Reason, State};
noreply(#state{} = State, [{Fun, Data} | Rest]) ->
    case Fun(State, Data) of
        #state{} = NewState ->
            noreply(NewState, Rest);
        {#state{} = NewState, Routines} ->
            noreply(NewState, Routines ++ Rest);
        _ ->
            noreply(State, Rest)
    end.

increase_elected(#state{elected = Elected} = State, []) ->
    State#state{elected = Elected + 1}.

add_candidates(#state{candidates = MyCandidates} = State, #sign{candidates = Candidates}) ->
    State#state{candidates = lists:usort(MyCandidates ++ Candidates)}.

set_role(State, Role) -> State#state{role = Role}.

set_leader(State, 'me') -> set_leader(State, sign(State));
set_leader(State, Leader) -> State#state{leader = Leader}.

leader(#state{leader = Leader}) -> Leader.

name(#state{name = Name}) -> Name;
name(#sign{name = Name}) -> Name.

set_callback_state(#state{} = State, CallbackState) -> State#state{callback_state = CallbackState}.

callback_state(#state{callback_state = State}) -> State.

leader_sync(#state{leader = Leader}) -> sign_sync(Leader).
sign_sync(#sign{sync = Sync}) -> Sync;
sign_sync(_) -> 'undefined'.

announce_leader(State, {To, 'me'}) ->
    announce_leader(State, {To, sign(State)});
announce_leader(State, {To, #sign{} = From}) ->
    send({State, To}, #?MODULE{from = From, msg = {'leader', leader(State)}}).

send(Pid, Msg) when is_atom(Pid); node() =:= erlang:node(Pid); node() =:= element(2, Pid) ->
    lager:debug("local message ~p: ~p", [Msg, Pid]),
    Pid ! Msg;
send({Name, Node}, Msg) when is_atom(Name) andalso is_atom(Node) ->
    Route = wapi_leader:route(Name, Node),
    send(Route, Msg);
send({#state{name = Name}, Node}, Msg) ->
    send({Name, Node}, Msg);
send({Name, #sign{node = Node}}, Msg) ->
    send({Name, Node}, Msg);
send(Pid, Msg) when is_pid(Pid) ->
    Route = wapi_leader:route(Pid),
    send(Route, Msg);
send(#sign{name = Name, node = Node}, Msg) ->
    send({Name, Node}, Msg);
send(Route, Msg) when is_binary(Route) ->
    lager:debug("amqp message ~p: ~p", [Msg, Route]),
    Props = [{<<"Message">>, wh_util:to_hex_binary(erlang:term_to_binary(Msg))}
             | wh_api:default_headers(<<"leader">>, <<"message">>, ?APP_NAME, ?APP_VERSION)
            ],
    try
        asd:asd()
    catch _:_ -> wh_util:log_stacktrace() end,
    wapi_leader:publish_req(Route, Props).

reply({Pid, _} = From, Reply) when is_pid(Pid) andalso erlang:node(Pid) =:= node() ->
    gen_server:reply(From, Reply);
reply({From, Tag}, Reply) ->
    send(From, {Tag, Reply}).

node({Name, Node}) when is_atom(Node), is_atom(Name) -> Node;
node(#sign{node = Node}) when is_atom(Node) -> Node;
node(Pid) when is_pid(Pid) -> erlang:node(Pid).

sign(#state{elected = Elected, restarted = Restarted, name = Name, candidates = Candidates} = State) ->
    #sign{elected = Elected
          ,restarted = -Restarted
          ,name = Name
          ,sync = leader_sync(State)
          ,candidates = Candidates
         }.
