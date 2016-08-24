%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_maintenance).

-include("fmc.hrl").

-export([accounts_list_by_device_id/1
         ,fmc_devices_list_by_account_id/1
         ,normalize_fmc_numbers/0]).

-define(DEVICES_LIST_ROW_FORMAT, " ~32.s | ~32.s |~n").
-define(ACCOUNTS_LIST_ROW_FORMAT, " ~32.s | ~32.s |~n").

-spec fmc_devices_list_by_account_id(api_binary()) -> 'ok'.
fmc_devices_list_by_account_id(AccountId) ->
    ViewOpts = [{'key', AccountId}],
    case couch_mgr:get_results(?WH_FMC_DB, <<"fmc_devices/list_by_account_id">>, ViewOpts) of
        {'ok', []} -> io:format("no devices were found for this account id~n");
        {'error', _E} -> io:format("fmc devices lookup error: ~p~n", [_E]);
        {'ok', Elems} when is_list(Elems) ->
            io:format(?DEVICES_LIST_ROW_FORMAT, [<<"FMC Document ID">>
                                                 ,<<"Device ID">>]),
            _ = [io:format(?DEVICES_LIST_ROW_FORMAT, [wh_doc:id(A)
                                                      ,wh_json:get_value([<<"value">>, <<"device_id">>], A)
                                                     ]) || A <- Elems],
            'ok'
    end.

-spec accounts_list_by_device_id(api_binary()) -> 'ok'.
accounts_list_by_device_id(DeviceId) ->
    ViewOpts = [{'key', DeviceId}],
    case couch_mgr:get_results(?WH_FMC_DB, <<"fmc_devices/list_by_device_id">>, ViewOpts) of
        {'ok', []} -> io:format("no accounts were found for this device id~n");
        {'error', _E} -> io:format("accounts lookup error: ~p~n", [_E]);
        {'ok', Elems} when is_list(Elems) ->
            io:format(?ACCOUNTS_LIST_ROW_FORMAT, [<<"FMC Document ID">>
                                                  ,<<"Account ID">>]),
            _ = [io:format(?ACCOUNTS_LIST_ROW_FORMAT, [wh_doc:id(A)
                                                       ,wh_json:get_value([<<"value">>, <<"account_id">>], A)
                                                      ]) || A <- Elems],
            'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Normalizes all A-Numbers in all records of "fmc" databases. Usually
%% you don't need to call it directly, because all A-Numbers already
%% should be normalized.
%% NOTE: The function used separade view "fmc_devices/full_crossbar_listing"
%% because it returns list of FMC items with _rev field which needed
%% to change a doc. The _rev fiels need only in this function and
%% shouldn't be exposed to any other REST API.
%% @end
%%--------------------------------------------------------------------
-spec normalize_fmc_numbers() -> 'ok'.
normalize_fmc_numbers() ->
    case couch_mgr:get_results(?WH_FMC_DB, <<"fmc_devices/full_crossbar_listing">>) of
        {'ok', []} -> io:format("fmc database is empty~n");
        {'error', _E} -> io:format("fmc devices lookup error: ~p~n", [_E]);
        {'ok', Elems} when is_list(Elems) ->
            NormalizedElems = [begin
                                   Item = wh_json:get_value(<<"value">>, E),
                                   NormalizedNumber = wnm_util:normalize_number(wh_json:get_value(<<"a_number">>, Item)),
                                   wh_json:set_values([{<<"a_number">>, NormalizedNumber}
                                                       ,{<<"pvt_type">>, <<"fmc_device">>}]
                                                       ,Item)
                               end || E <- Elems],
            case couch_mgr:save_docs(?WH_FMC_DB, NormalizedElems) of
                {'ok', _} ->
                    'ok';
                {'error', _E} ->
                    io:format("error while save docs: ~p~n", [_E])
            end
    end.
