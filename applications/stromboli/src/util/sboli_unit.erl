%%%-------------------------------------------------------------------
%%% @author ivan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. May 2016 16:26
%%%-------------------------------------------------------------------
-module(sboli_unit).
-author("Ivan Romanyuk").

%% API
-export([get_lines/1
         ,get_profiles/1
         ,get_unit_json/1
         ,get_unit_by_mac/2
         ,get_account_id/1
         ,get_config/1
        ]).

-include("../stromboli.hrl").

-define(LISTING_BY_MAC, <<"stromboli/units_listing_by_mac">>).


-spec get_lines(wh_json:object()) -> wh_json:objects().
get_lines(JObj) ->
    Lines = wh_json:get_value(<<"lines">>, JObj),
    AccountId = get_account_id(JObj),
    [begin
         DevId = wh_json:get_value(<<"device">>, Line),
         Device = get_device(AccountId, DevId),
         wh_json:set_value(<<"device">>, Device, Line)
     end  || Line <- Lines].

-spec get_device(ne_binary(), ne_binary()) -> wh_json:object().
get_device(AccountId, DeviceId) ->
    AccountDb = wh_util:format_account_db(AccountId),
    {'ok', JObj} = couch_mgr:open_doc(AccountDb, DeviceId),
    JObj.

-spec get_profiles(wh_json:object()) -> wh_json:objects().
get_profiles(JObj) ->
    wh_json:get_value(<<"profiles">>, JObj).

-spec get_unit_json(stromboli:mac()) ->
                    {'error', 'not_found'}
                    | couch_mgr:couchbeam_error()
                    | wh_json:object().
get_unit_json(Mac) ->
    {'ok', JObjs} = couch_mgr:get_results(?WH_ACCOUNTS_DB, ?LISTING_BY_MAC),
    case wh_json:find_value(<<"key">>, Mac, JObjs) of
        'undefined' -> {'error', 'not_found'};
        JObj -> AccountDb = wh_json:get_value([<<"value">>, <<"account_db">>], JObj),
            UnitId = wh_json:get_value([<<"value">>, <<"unit_id">>], JObj),
            couch_mgr:open_doc(AccountDb, UnitId)
    end.

-spec get_unit_by_mac(stromboli:mac(), ne_binary()) ->
                        {'ok', ne_binary()}
                        | {'error', 'not_found'}.
get_unit_by_mac(Mac, AccId) ->
    AccountDb = wh_util:format_account_db(AccId),
    {'ok', JObjs} = couch_mgr:get_results(AccountDb, ?LISTING_BY_MAC),
    find_by_mac(Mac, JObjs).

-spec get_account_id(wh_json:obeject()) -> ne_binary().
get_account_id(JObj) ->
    wh_json:get_value(<<"pvt_account_id">>, JObj).

-spec find_by_mac(stromboli:mac(), wh_json:objects()) ->
                    {'ok', ne_binary()}
                    | {'error', 'not_found'}.
find_by_mac(Mac, [JObj|Rest]) ->
    LMac = binstr:to_lower(Mac),
    case binstr:to_lower(wh_json:get_value(<<"key">>, JObj)) of
        LMac -> {'ok', wh_json:get_value(<<"value">>, JObj)};
        _ -> find_by_mac(Mac, Rest)
    end;
find_by_mac(_Mac, []) -> {'error', 'not_found'}.

-spec get_config(wh_json:object()) -> wh_json:object().
get_config(JObj) ->
    wh_json:get_value(<<"config">>, JObj).