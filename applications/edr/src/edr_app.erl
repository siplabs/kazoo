%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2016
%%% @contributors
%%%   SIPLABS, LLC (Vorontsov Nikita)
%%%-------------------------------------------------------------------
-module(edr_app).

-behaviour(application).

-include("edr.hrl").

-export([start/2
         ,stop/1
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(any(), any()) ->
                   {'ok', pid()} |
                   {'error', startlink_err()}.
start(_Type, _Args) -> edr:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) -> edr:stop().
