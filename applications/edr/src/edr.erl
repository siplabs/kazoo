%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2016
%%% @contributors
%%%   SIPLABS, LLC (Vorontsov Nikita)
%%%-------------------------------------------------------------------
-module(edr).

-include_lib("edr.hrl").

-export([start_link/0
         ,stop/0
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = declare_exchanges(),
    _ = start_deps(),
    edr_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- ['whistle_amqp', 'whistle_couch']],
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    wapi_edr:declare_exchanges().
