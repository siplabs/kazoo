%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is worker pool manager. It is used to create workers for a requests from cm_listener.
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_pool_mgr).
-behaviour(gen_server).

-include("circlemaker.hrl").

-record(state, {workers = [] :: pids()}).

-type pool_mgr_state() :: #state{}.
-type authn_response() :: {'ok', 'aaa_mode_off'} | {'ok', tuple()}.

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
         ,start_link/0
         ,send_authn_response/4
         ,do_request/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Makes an AAA request
%% @end
%%--------------------------------------------------------------------
-spec do_request(wh_json:object()) -> 'ok'.
do_request(Request) ->
    lager:debug("Make AAA request: ~p", [Request]),
    gen_server:cast(?MODULE, {'request', Request}).

%%--------------------------------------------------------------------
%% @doc
%% Stores a new worker process into the Pool Manager's state
%% @end
%%--------------------------------------------------------------------
-spec insert_worker(pid(), pool_mgr_state()) -> pool_mgr_state().
insert_worker(Worker, State) ->
    lager:debug("New worker ~p stored", [Worker]),
    State1 = State#state{workers=[Worker | State#state.workers]},
    lager:debug("Workers count is ~p", [length(State#state.workers)]),
    State1.

%%--------------------------------------------------------------------
%% @doc
%% Removes a new worker process from the Pool Manager's state
%% @end
%%--------------------------------------------------------------------
-spec remove_worker(pid(), pool_mgr_state()) -> pool_mgr_state().
remove_worker(Worker, State) ->
    lager:debug("Worker ~p removed", [Worker]),
    State1 = State#state{workers=lists:delete(Worker, State#state.workers)},
    lager:debug("Workers count is ~p", [length(State#state.workers)]),
    State1.

%%--------------------------------------------------------------------
%% @doc
%% Handler for a response message for AuthN
%% @end
%%--------------------------------------------------------------------
-spec send_authn_response(pid(), authn_response(), wh_json:object(), pid()) -> 'ok'.
send_authn_response(SenderPid, Response, JObj, Self) ->
    lager:debug("Response ~p is prepared to send to worker ~p", [Response, Self]),
    gen_server:cast(SenderPid, {'response', Response, JObj, Self}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', tuple()}.
init([]) ->
    lager:debug("Pool manager started"),
    {'ok', #state{}}.

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
    {'noreply', State}.

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
handle_cast({'request', JObj}, State) ->
    lager:debug("requst message is ~p", [JObj]),
    {'noreply', dist_workers(JObj, State)};
handle_cast({'response', Response, JObj, Worker}, State) ->
    lager:debug("response message is ~p", [Response]),
    NewState = remove_worker(Worker, State),
    poolboy:checkin(?WORKER_POOL, Worker),
    Result = case Response of
                 {'ok', {'radius_request', _, 'accept', _, _, _, _, _}} ->
                     <<"accept">>;
                 _ ->
                     <<"reject">>
             end,
    Queue = wh_json:get_value(<<"Response-Queue">>, JObj),
    Password = wh_json:get_value(<<"User-Password">>, JObj),
    JObj1 = wh_json:set_values([{<<"AAA-Result">>, Result},
                                {<<"Auth-Password">>, Password},
                                {<<"Event-Name">>,<<"aaa_authn_resp">>}],
                                JObj),
    % send response to the registrar_listener queue
    wapi_aaa:publish_resp(Queue, JObj1),
    {'noreply', NewState};
handle_cast(Message, State) ->
    lager:debug("unexpected message is ~p", [Message]),
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
handle_info(Info, State) ->
    lager:debug("unexpected info message is ~p", [Info]),
    {'noreply', State}.

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
terminate(Reason, _State) ->
    lager:debug("Circlemaker worker terminating: ~p", [Reason]).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts new worker for processing a request
%% @end
%%--------------------------------------------------------------------
-spec dist_workers(wh_json:object(), pool_mgr_state()) -> pool_mgr_state().
dist_workers(JObj, State) ->
    lager:debug("Trying to start new worker..."),
    case catch poolboy:checkout(?WORKER_POOL) of
        Worker when is_pid(Worker) ->
            lager:debug("Worker started sucessfully"),
            cm_worker:send_authn_req(Worker, JObj),
            insert_worker(Worker, State);
        Else ->
            lager:error("Failed to start a worker. Reason is ~p", [Else]),
            State
    end.
