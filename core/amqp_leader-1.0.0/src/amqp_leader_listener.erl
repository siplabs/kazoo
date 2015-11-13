%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(amqp_leader_listener).

-behaviour(gen_listener).

-export([is_ready/0]).
-export([start_link/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_leader.hrl").

-record(state, {self = self(), name, pending = [], is_connected = 'false', is_up = 'false'}).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(Name), [{'leader', [{'name', Name}]}
                        ]).
-define(RESPONDERS, []).
-define(QUEUE_NAME(Name), wapi_leader:queue(Name)).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================
is_ready() ->
    {'registered_name', Name} = erlang:process_info(self(), 'registered_name'),
    Sup = element(2, hd([X || X <- supervisor:which_children(amqp_leader_sup), element(1, X) =:= Name])),
    lager:debug("sup ~p", [Sup]),
    Pid = element(2, hd([X || X <- supervisor:which_children(Sup), element(1, X) =:= ?MODULE])),
    lager:debug("pid ~p", [Pid]),
    gen_listener:call(Pid, 'is_ready').

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_listener:start_link(?MODULE, [
                                      {'bindings', ?BINDINGS(Name)}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME(Name)}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ], [Name]).

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
init([Name]) ->
    wh_util:put_callid(wapi_leader:queue(Name, node())),
    wh_nodes:notify_expire(),
    case ets:match(wh_nodes, #wh_node{node = node(), expires = '$2', _ = '_'}) of
        [] ->
            wh_nodes:notify_new(),
            {'ok', #state{self = self(), name = Name}};
        [_] ->
            {'ok', #state{self = self(), name = Name, is_up = 'true'}}
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
handle_call('is_ready', From, #state{pending = Pids} = State) ->
    NewState = maybe_ready(State#state{pending = [From | Pids]}),
    {'noreply', NewState};
handle_call(_Request, _From, State) ->
    lager:warning("unhandled call ~p", [_Request]),
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
handle_cast({'wh_nodes', {'expire', #wh_node{node = Node}}}, #state{name = Name} = State) ->
    Name ! {ldr, 'DOWN', Node},
    {'noreply', State};
handle_cast({'wh_nodes', {'new', #wh_node{node = Node}}}, State) when Node =:= node() ->
    NewState = maybe_ready(State#state{is_up = 'true'}),
    {'noreply', NewState};
handle_cast({'wh_nodes', {'new', _Node}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    NewState = maybe_ready(State#state{is_connected = 'true'}),
    {'noreply', NewState#state{pending = []}};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:warning("unhandled cast ~p", [_Msg]),
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
    lager:warning("~s unhandled info ~p", [node(), _Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(JObj, #state{name = Name}) ->
    wh_util:put_callid(wapi_leader:queue(Name, node())),
    NodeBin = iolist_to_binary(io_lib:format("~s", [node()])),
    lager:debug([{trace, true}], "msg ~p", [JObj]),
    case wh_json:get_value(<<"Node">>, JObj) of
        NodeBin -> lager:debug([{trace, true}], "message discarded");
        _ ->
            Msg = erlang:binary_to_term(wh_util:from_hex_binary(wh_json:get_value(<<"Message">>, JObj))),
            lager:debug([{trace, true}], "event for ~p ~p", [Name, Msg]),
            Name ! Msg
    end,
    {'reply', []}.

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
maybe_ready(#state{pending = Pids
                   ,is_up = 'true'
                   ,is_connected = 'true'
                  } = State) ->
    [gen_server:reply(Pid, 'true') || Pid <- Pids],
    State#state{pending = []};
maybe_ready(State) ->
    case State#state.is_up of
        'true' -> lager:debug("not connected");
        'false' -> lager:debug("node is down")
    end,
    State.
