%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_route_win).

-export([handle_req/2]).

-include("fmc.hrl").

handle_req(JObj, Props) ->
    lager:info("FMC trying to handle route_win ..."),
    lager:debug("FMC JObj is ~p", [JObj]),
    lager:debug("FMC Props is ~p", [Props]),
    'true' = wapi_route:win_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put('callid', CallId),
    case whapps_call:retrieve(CallId, ?FMC_ROUTE_REQ_SECTION) of
        {'ok', C} ->
            lager:debug("FMC wins the routing"),
            Call = whapps_call:from_route_win(JObj, C),
            {FmcRec, RouteReqJObj} = whapps_call:kvs_fetch(<<"fmc_action">>, Call),
            lager:info("Used FmcRec is ~p", [FmcRec]),
            MsgId = wh_json:get_value(<<"Msg-ID">>, RouteReqJObj),
            fmc_ets:put(MsgId, CallId, C),
            UpdatedCall = whapps_call:kvs_store(<<"fmc_action_win">>, JObj, Call),
            whapps_call:cache(UpdatedCall, ?FMC_ROUTE_WIN_SECTION),
            maybe_rewrite_headers(FmcRec, RouteReqJObj, Props);
        {'error', _R} ->
            lager:error("something went wrong: ~p", [_R])
    end.

maybe_rewrite_headers(FmcRec, RouteReqJObj, Props) ->
    lager:debug("maybe_rewrite_headers called"),
    lager:debug("FMC record on rewrite is ~p", [FmcRec]),
    lager:debug("FMC RouteReqJObj on rewrite is ~p", [RouteReqJObj]),
    FmcDeviceId = wh_json:get_value(<<"device_id">>, FmcRec),
    FmcAccountId = wh_json:get_value(<<"account_id">>, FmcRec),
    _FmcANumber = wh_json:get_value(<<"a_number">>, FmcRec),
    _FmcXFmcValue = wh_json:get_value(<<"x_fmc_value">>, FmcRec),
    {'ok', AccountDoc} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, FmcAccountId),
    AaaDb = wh_util:format_account_id(FmcAccountId, 'encoded'),
    {'ok', DeviceDoc} = couch_mgr:open_cache_doc(AaaDb, FmcDeviceId),
    OwnerId = wh_json:get_value(<<"owner_id">>, AaaDb),
    RouteReqJObj1 = case OwnerId of
                        'undefined' ->
                            RouteReqJObj;
                        _OwnerId ->
                            {'ok', OwnerDoc} = couch_mgr:open_cache_doc(AaaDb, OwnerId),
                            UserName = wh_json:get_value(<<"username">>, OwnerDoc),
                            wh_json:set_values([{[?CCV, <<"Owner-ID">>], OwnerId}
                                                ,{[?CCV, <<"Username">>], UserName}]
                                                ,RouteReqJObj)
                    end,
    % set headers: Account-ID, Device-ID and User-ID, Authorizing-ID, Authorizing-Type
    FMCDeviceType = wh_json:get_value(<<"pvt_type">>, DeviceDoc),
    AccountName = wh_json:get_value(<<"name">>, AccountDoc),
    AccountRealm = wh_json:get_value(<<"realm">>, AccountDoc),

    RouteReqJObj2 = wh_json:set_values([{[?CCV, <<"Account-ID">>], FmcAccountId}
                                        ,{[?CCV, <<"Account-Name">>], AccountName}
                                        ,{[?CCV, <<"Account-Realm">>], AccountRealm}
                                        ,{[?CCV, <<"Realm">>], AccountRealm}
                                        ,{[?CCV, <<"Authorizing-ID">>], FmcDeviceId}
                                        ,{[?CCV, <<"Authorizing-Type">>], FMCDeviceType}
                                        ,{[?CCV, <<"Originator-Type">>], ?PLATFORM_ORIGINATOR}]
                                       ,RouteReqJObj1),
    % remove FMC specific headers
    RouteReq = wh_json:delete_key([[<<"Custom-SIP-Headers">>, ?XFMC_HEADER]]
                                  ,RouteReqJObj2),
    % send to all route_req handlers
    lager:debug("Rewritten request is ~p", [RouteReq]),
    maybe_send_route_req(RouteReq, Props).

maybe_send_route_req(RouteReqJObj, Props) ->
    lager:debug("maybe_send_route_req called"),
    ControllerQ = props:get_value('queue', Props),
    RouteReqJObj1 = wh_json:set_values(wh_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION), RouteReqJObj),
    lager:debug("Request before send is ~p", [RouteReqJObj1]),
    wapi_route:publish_req(RouteReqJObj1).