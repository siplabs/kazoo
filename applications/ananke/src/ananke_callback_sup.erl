%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(ananke_callback_sup).

-export([init/1
        ,start_link/0
        ,start_child/4
        ,delete_child/1
        ]).

-include("ananke.hrl").

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec start_child(any(), wh_proplist(), integer(), integer()) ->
    sup_startchild_ret().
start_child(Id, OriginateReq, Attempts, Interval) ->
    case supervisor:start_child(?MODULE, {Id
                                          ,{'ananke_callback_wrkr', 'start_link'
                                            ,[OriginateReq, Attempts, Interval]}
                                          ,'transient'
                                          ,'brutal_kill'
                                          ,'worker'
                                          ,['ananke_voicemail_call_wrkr']
                                         })
    of
        {'error', 'already_present'} ->
            supervisor:delete_child(?MODULE, Id),
            start_child(Id, OriginateReq, Attempts, Interval);
        Reply -> Reply
    end.

-spec delete_child(ne_binary()) -> 'ok' | {'error', any()}.
delete_child(Number) ->
    supervisor:delete_child(?MODULE, Number).

-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, []}}.
