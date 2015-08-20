%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cccp_maintenance).

-include("cccp.hrl").

%% API
-export([list_blocked_cids/0
         ,clear_blocking/1
        ]).

-spec list_blocked_cids() -> 'ok'.
list_blocked_cids() ->
    ViewOpts = [{'descending', 'false'}
                ,{'startkey', wh_util:current_tstamp()}
               ],
    case couch_mgr:get_results(?KZ_CCCPS_DB, cccp_blocking:blocking_expiration(), ViewOpts) of
        {'ok', JObjs} -> lists:foreach(fun print_cid_status/1, JObjs);
        _Err -> io:format("Error: ~p!~n", [_Err])
    end,
    'ok'.

-spec print_cid_status(wh_json:object()) -> 'ok'.
print_cid_status(JObj) ->
    CID = wh_json:get_value(<<"id">>, JObj),
    Till = case wh_json:get_value(<<"key">>, JObj) of
               <<"forever">> = V -> V;
               Timestamp -> wh_util:format_datetime(Timestamp)
           end,
    io:format("~-20s~s~n", [CID, Till]).

-spec clear_blocking(ne_binary()) -> 'ok' | 'fail'.
clear_blocking(CID) ->
    case cccp_blocking:clear_blocking(CID) of
        {'ok', _} -> 'ok';
        _Err ->
            io:format("Error: ~p!~n", [_Err]),
            'fail'
    end.
