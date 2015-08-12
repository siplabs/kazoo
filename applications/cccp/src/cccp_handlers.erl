%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(cccp_handlers).

-export([handle_route_req/2
         ,handle_route_win/2
        ]).

-include("cccp.hrl").

-spec handle_route_req(wh_json:object(), wh_proplist()) -> any().
handle_route_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),
    lager:info("CCCP has received a route req, shold we handle the call?"),

    Call = whapps_call:from_route_req(JObj),
    CB_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cb_number">>)),
    CC_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cc_number">>)),

    case wnm_util:normalize_number(whapps_call:request_user(Call)) of
        CB_Number -> park_call(JObj, Props, Call);
        CC_Number -> park_call(JObj, Props, Call);
        _ -> 'ok'
    end.

-spec park_call(wh_json:object(), wh_proplist(), whapps_call:call()) -> 'ok'.
park_call(JObj, Props, Call) ->
    lager:info("Let's park it!"),
    Q = props:get_value('queue', Props),
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    whapps_call:cache(Call, ?APP_NAME).

-spec handle_route_win(wh_json:object(), wh_proplist()) -> 'ok'.
handle_route_win(JObj, Props) ->
    'true' = wapi_route:win_v(JObj),
    lager:info("CCCP has received a route win, taking control of the call"),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case whapps_call:retrieve(CallId, ?APP_NAME) of
        {'ok', Call} ->
            Call1 = whapps_call:kvs_store('consumer_pid', self(), whapps_call:from_route_win(JObj, Call)),
            Call2 = whapps_call:kvs_store('server_pid', props:get_value('server', Props), Call1),
            whapps_call:cache(Call2, ?APP_NAME),
            handle_cccp_call(Call2);
        {'error', _R} ->
            lager:debug("Unable to find call record during route_win")
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_cccp_call(whapps_call:call()) -> 'ok'.
handle_cccp_call(Call) ->
    CID = wnm_util:normalize_number(whapps_call:caller_id_number(Call)),
    CB_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cb_number">>)),
    CC_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cc_number">>)),
    case wnm_util:normalize_number(whapps_call:request_user(Call)) of
        CB_Number ->
            handle_callback(CID, Call);
        CC_Number ->
            cccp_platform_sup:new(Call)
    end.

-spec handle_callback(ne_binary(), whapps_call:call()) -> 'ok'.
handle_callback(CallerNumber, Call) ->
    whapps_call_command:hangup(Call),
    case cccp_auth:authorize(CallerNumber, cccp_util:cid_listing()) of
        {'ok',Auth} ->
            JObj = wh_json:from_list([{<<"Number">>, CallerNumber}
                                      ,{<<"Account-ID">>, cccp_auth:account_id(Auth)}
                                      ,{<<"Outbound-Caller-ID-Number">>, cccp_auth:outbound_cid(Auth)}
                                      ,{<<"Auth-Doc-Id">>, cccp_auth:auth_doc_id(Auth)}
                                     ]),
            timer:sleep(2000),
            cccp_callback_sup:new(JObj);
        E ->
            lager:info("No caller information found for ~p. Won't call it back. (~p)", [CallerNumber, E])
    end.
