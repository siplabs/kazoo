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

-spec handle_req(wh_json:object(), wh_json:proplist()) -> 'ok' | {'error', any()}.
handle_req(JObj, Props) ->
    lager:info("fmc trying to handle route_win ..."),
    'true' = wapi_route:win_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    wh_util:put_callid(CallId),
    case whapps_call:retrieve(CallId, ?FMC_ROUTE_REQ_SECTION) of
        {'ok', C} ->
            Call = whapps_call:from_route_win(JObj, C),
            {FmcRec, RouteReqJObj} = whapps_call:kvs_fetch(<<"fmc_action">>, Call),
            MsgId = wh_json:get_value(<<"Msg-ID">>, RouteReqJObj),
            fmc_ets:put(MsgId, CallId, C),
            UpdatedCall = whapps_call:kvs_store(<<"fmc_action_win">>, JObj, Call),
            whapps_call:cache(UpdatedCall, ?FMC_ROUTE_WIN_SECTION),
            maybe_rewrite_headers(FmcRec, RouteReqJObj, Props);
        {'error', _R} ->
            lager:error("something went wrong: ~p", [_R])
    end.

-spec maybe_rewrite_headers(wh_json:object(), wh_json:object(), wh_json:proplist()) -> 'ok'.
maybe_rewrite_headers(FmcRec, RouteReqJObj, Props) ->
    FmcDeviceId = wh_json:get_value(<<"device_id">>, FmcRec),
    FmcAccountId = wh_json:get_value(<<"account_id">>, FmcRec),
    RouteReq = build_route_request(RouteReqJObj, FmcDeviceId, FmcAccountId),
    % send to all route_req handlers
    maybe_send_route_req(RouteReq, Props).

-spec build_route_request(wh_json:object(), api_binary(), api_binary()) -> wh_json:object().
build_route_request(RouteReqJObj, FmcDeviceId, FmcAccountId) ->
    {'ok', AccountDoc} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, FmcAccountId),
    AccountDbName = wh_util:format_account_id(FmcAccountId, 'encoded'),
    {'ok', DeviceDoc} = couch_mgr:open_cache_doc(AccountDbName, FmcDeviceId),
    OwnerId = wh_json:get_value(<<"owner_id">>, DeviceDoc),

    RouteReqJObj1 = case OwnerId of
                        'undefined' ->
                            RouteReqJObj;
                        _OwnerId ->
                            {'ok', OwnerDoc} = couch_mgr:open_cache_doc(AccountDbName, OwnerId),
                            UserName = wh_json:get_value(<<"username">>, OwnerDoc),
                            wh_json:set_values([{?CCV(<<"Owner-ID">>), OwnerId}
                                                ,{?CCV(<<"Username">>), UserName}]
                                               ,RouteReqJObj)
                    end,
    % set headers: Account-ID, Device-ID and User-ID, Authorizing-ID, Authorizing-Type
    FMCDeviceType = wh_json:get_value(<<"pvt_type">>, DeviceDoc),
    AccountName = kz_account:name(AccountDoc),
    AccountRealm = kz_account:realm(AccountDoc),

    RouteReqJObj2 = wh_json:set_values([{?CCV(<<"Account-ID">>), FmcAccountId}
                                        ,{?CCV(<<"Account-Name">>), AccountName}
                                        ,{?CCV(<<"Account-Realm">>), AccountRealm}
                                        ,{?CCV(<<"Realm">>), AccountRealm}
                                        ,{?CCV(<<"Authorizing-ID">>), FmcDeviceId}
                                        ,{?CCV(<<"Authorizing-Type">>), FMCDeviceType}
                                        ,{?CCV(<<"Originator-Type">>), ?PLATFORM_ORIGINATOR}
                                       ], RouteReqJObj1),
    % remove FMC specific headers
    wh_json:delete_key([[<<"Custom-SIP-Headers">>, ?XFMC_HEADER]], RouteReqJObj2).

-spec maybe_send_route_req(wh_json:object(), wh_json:proplist()) -> 'ok'.
maybe_send_route_req(RouteReqJObj, Props) ->
    ControllerQ = props:get_value('queue', Props),
    RouteReqJObj1 = wh_json:set_values(wh_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION), RouteReqJObj),
    wapi_route:publish_req(RouteReqJObj1).
