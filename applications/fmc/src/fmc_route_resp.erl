%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_route_resp).

-export([handle_req/2]).

-include("fmc.hrl").

-spec handle_req(wh_json:object(), wh_json:proplist()) -> any().
handle_req(JObj, _Props) ->
    lager:info("fmc trying to handle route_resp ..."),
    'true' = wapi_route:resp_v(JObj),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    case wh_json:get_value(<<"App-Name">>, JObj) of
        <<"callflow">> ->
            [{_MsgId, CallId, _Call}] = fmc_ets:get(MsgId),
            wh_util:put_callid(CallId),
            process_route_resp(JObj, CallId),
            fmc_ets:delete(MsgId);
       _AppName ->
            % it's a route_resp from another app, so bypass it
            'ok'
    end.

-spec process_route_resp(wh_json:object(), ne_binary()) -> 'ok'.
process_route_resp(JObj, CallId) ->
    case whapps_call:retrieve(CallId, ?FMC_ROUTE_WIN_SECTION) of
        {'ok', C} ->
            lager:info("starting to prepare route_win for callflow"),
            WinJObj = whapps_call:kvs_fetch(<<"fmc_action_win">>, C),
            Q = wh_json:get_value(<<"Control-Queue">>, WinJObj),
            CCVs = wh_json:get_value(?CCV_VALUE, WinJObj),
            % create req_win header to send it to callflow
            ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
            RouteWin = [{<<"Msg-ID">>, CallId}
                        ,{<<"Call-ID">>, CallId}
                        ,{<<"Control-Queue">>, Q}
                        ,{?CCV_VALUE, CCVs}
                        | wh_api:default_headers(ServerId
                                                 ,<<"dialplan">>
                                                 ,<<"route_win">>
                                                 ,?APP_NAME
                                                 ,?APP_VERSION
                                                )
                       ],
            wapi_route:publish_win(ServerId, RouteWin);
        {'error', _R} ->
            lager:error("something went wrong: ~p", [_R])
    end.