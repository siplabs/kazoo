%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_route_req).

-include("fmc.hrl").

-export([handle_req/2
         ,find_fmc_item/2
        ]).

-spec handle_req(wh_json:object(), wh_json:proplist()) -> any().
handle_req(JObj, Props) ->
    lager:info("fmc tried to handle route_req"),
    'true' = wapi_route:req_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    wh_util:put_callid(CallId),

    ANumber = wh_json:get_binary_value(<<"Caller-ID-Number">>, JObj),
    FMCHeaderValue = wh_json:get_binary_value([<<"Custom-SIP-Headers">>, ?XFMC_HEADER], JObj),
    maybe_fmc_header_value(FMCHeaderValue, ANumber, JObj, Props).

-spec maybe_fmc_header_value(binary() | 'undefined', binary(), wh_json:object(), wh_json:proplist()) -> any().
maybe_fmc_header_value('undefined', _ANumber, _JObj, _Props) ->
    lager:info("fmc does not know what to do with this!");
maybe_fmc_header_value(FMCHeaderValue, ANumber, JObj, Props) ->
    Call = whapps_call:from_route_req(JObj),
    ExtractedFMCValue = case re:run(FMCHeaderValue, ?XFMC_REGEXP, [{'capture', [1], 'binary'}]) of
                            'nomatch' -> 'undefined';
                            {'match', [Val]} -> Val
                        end,
    %% do magic to determine if we should respond...
    %% update the call kvs with which module to use (tone or echo)
    case find_fmc_item(ANumber, ExtractedFMCValue) of
        {'ok', Action} ->
            lager:info("fmc record found"),
            ControllerQ = props:get_value('queue', Props),
            case send_route_response(ControllerQ, JObj) of
                'ok' ->
                    UpdatedCall = whapps_call:kvs_store(<<"fmc_action">>, {Action, JObj}, Call),
                    whapps_call:cache(UpdatedCall, ?FMC_ROUTE_REQ_SECTION);
                {'error', _E} ->
                    lager:info("fmc cannot send route response: ~p", [_E])
            end;
        {'error', _E} ->
            lager:info("fmc does not know what to do with this! error is ~p", [_E])
    end.

-spec send_route_response(ne_binary(), wh_json:json()) -> 'ok' | {'error', any()}.
send_route_response(ControllerQ, JObj) ->
    lager:info("fmc knows how to route the call! sent park response"),
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher).

-spec find_fmc_item(api_binary(), api_binary()) -> {'ok', wh_json:object()} | {'error', any()}.
find_fmc_item('undefined', 'undefined') ->
    lager:error("error: a-number and fmc value are not defined"),
    {'error', 'a_number_and_fmc_value_undefined'};
find_fmc_item('undefined', FMCValue) ->
    lager:error("error: a-number is not defined and fmc value is ~p", [FMCValue]),
    {'error', 'a_number_undefined'};
find_fmc_item(ANumber, 'undefined') ->
    lager:error("error: fmc value is not defined and a-number is ~p", [ANumber]),
    {'error', 'fmc_value_undefined'};
find_fmc_item(ANumber, FMCValue) ->
    NormalizedANumber = wnm_util:normalize_number(ANumber),
    SearchVal = [FMCValue, NormalizedANumber],
    lager:debug("searching fmc record where fmc value is ~p and a-number is ~p", [FMCValue, NormalizedANumber]),
    case couch_mgr:get_results(?WH_FMC_DB, <<"fmc_devices/list_by_fmc_and_number">>
                               ,[{'startkey', SearchVal}, {'endkey', SearchVal}])
    of
        {'ok', []} ->
            {'error', 'not_found'};
        {'ok', [JObj]} ->
            {'ok', wh_json:get_json_value(<<"value">>, JObj)};
        {'ok', JObjs} when is_list(JObjs) ->
            lager:error("error: more than one fmc record with same data"),
            {'error', 'found_more_than_one'};
        {'error', _E} ->
            lager:error("error on find operation: ~p", [_E]),
            {'error', _E}
    end.
