%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_sup).

-behaviour(supervisor).

-include("fmc.hrl").

-export([init/1
         ,start_link/0
         ,find_srv/0
        ]).

-define(SUBS_ETS_OPTS, [{'table_id', fmc_ets:table_id()}
                        ,{'table_options', fmc_ets:table_config()}
                        ,{'find_me_function', fun ?MODULE:find_srv/0}
                       ]).

-define(CHILDREN, [?WORKER_NAME_ARGS('kazoo_etsmgr_srv', 'fmc_calls_tbl', [?SUBS_ETS_OPTS])
                   ,?WORKER('fmc_listener')]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec find_srv() -> pid() | 'undefined'.
find_srv() ->
    case [P || {_, P, 'worker', ['fmc_listener']} <- supervisor:which_children(?MODULE)] of
        [] -> 'undefined';
        [Pid] -> Pid
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    wh_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
