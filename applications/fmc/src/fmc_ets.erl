%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_ets).

-include("fmc.hrl").

%% API
-export([table_id/0
         ,table_config/0
         ,put/3
         ,delete/1
         ,get/1
        ]).

-spec table_id() -> 'ets_fmc_calls'.
table_id() -> 'ets_fmc_calls'.

-spec table_config() -> wh_proplist().
table_config() ->
    ['named_table', 'public'].

-spec put(api_binary(), api_binary(), tuple()) -> 'true'.
put(MsgId, CallID, Call) ->
    ets:insert(table_id(), {MsgId, CallID, Call}).

-spec get(api_binary()) -> list(tuple()).
get(MsgId) ->
    ets:lookup(table_id(), MsgId).

-spec delete(api_binary()) -> 'true'.
delete(MsgId) ->
    ets:delete(table_id(), MsgId).
