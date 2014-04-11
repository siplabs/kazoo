%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_trusted).

-export([
         pre_process_event/4 ,
         pre_process_routes/3, pre_process_routes/4
        ]).

-include("ecallmgr.hrl").


-spec pre_process_event(ne_binary(), api_binary(), wh_proplist(), atom()) -> wh_proplist().
pre_process_event(<<"CHANNEL_CREATE">>, UUID, Props, Node) ->
    AccountId = props:get_value(<<"variable_ecallmgr_Account-ID">>, Props),
    % lager:info("CHANNEL_CREATE ~p",[AccountId]),
    Kazoo = props:get_value(<<"variable_sip_h_X-Kazoo-Gsm">>, Props),
    % lager:info("CHANNEL_CREATE GSM ~p",[Kazoo]),   
    X2 = props:get_value(<<"variable_sip_user_agent">>, Props),
    %lager:info("CHANNEL_CREATE Agent ~p",[X2]),
    case {AccountId , Kazoo} of
        {_ , 'undefined'} -> Props;
        {'undefined', User } -> pre_process_routes(Node, UUID, Props);                        
        _Else -> Props
    end;
pre_process_event(_EventName, _UUID, Props, _Node) ->
    Props.
    

-spec pre_process_routes(atom(), ne_binary(), wh_proplist()) -> wh_proplist().
-spec pre_process_routes(atom(), ne_binary(), ne_binary(), wh_proplist()) -> wh_proplist().

pre_process_routes(Node, CallId, Props) ->
    pre_process_routes(Node, 'undefined', CallId, Props).

pre_process_routes(Node, FetchId, CallId, Props) ->
    case lists:member(wh_util:to_binary(Node), ecallmgr_config:get(<<"fs_trusted">>, [])) of
        'true' -> do_pre_process_routes(Node, FetchId, CallId, Props);
        _Else -> Props
    end.

do_pre_process_routes(Node, FetchId, CallId, Props) ->
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,authz_req(CallId, FetchId, Props, Node)
                                  ,fun wapi_trusted:publish_authz_req/1
                                  ,fun wapi_trusted:authz_resp_v/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive pre-route response for request ~s: ~p", [FetchId, _R]),
            Props;
        {'ok', JObj} ->
            CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
            NewProps = wh_json:to_proplist( CCVs),
            do_process_props(NewProps, Props, Node, CallId)
    end.

do_process_props(NewProps, OldProps, Node, UUID) ->
    %_ = ecallmgr_util:set(Node, CallId, NewProps),
    UpdateProps = lists:map(fun({K,V}) -> {<<?CHANNEL_VAR_PREFIX, K/binary>>,V} end, NewProps),
    SetProps = lists:map(fun({K,V}) -> {?GET_CCV(K),V} end, NewProps),
    % props:to_log(SetProps,<<"SET PROPS">>),
    ecallmgr_fs_command:set(Node, UUID, UpdateProps),
    Props2 = props:set_values(SetProps, OldProps),
    % props:to_log(Props2,<<"PROPS 2">>),
    Props2.
    

-spec authz_req(ne_binary(), ne_binary(), wh_proplist(), atom()) -> wh_proplist().
authz_req(CallId, FetchId, Props, Node) ->
    %props:to_log(Props, <<"ROUTE REQ">>),
    props:filter_undefined([{<<"Msg-ID">>, FetchId}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Caller-ID-Name">>, props:get_first_defined([<<"variable_effective_caller_id_name">>
                                                      ,<<"Caller-Caller-ID-Name">>
                                                     ], Props, <<"Unknown">>)}
     ,{<<"Caller-ID-Number">>, props:get_first_defined([<<"variable_effective_caller_id_number">>
                                                        ,<<"Caller-Caller-ID-Number">>
                                                       ], Props, <<"0000000000">>)}
     ,{<<"From-Network-Addr">>, props:get_first_defined([<<"variable_sip_h_X-AUTH-IP">>
                                                         ,<<"variable_sip_received_ip">>
                                                        ], Props)}
     ,{<<"Access-Network-Info">>, props:get_value(<<"variable_sip_h_P-Access-Network-Info">>, Props)}
     ,{<<"Physical-Info">>, props:get_value(<<"variable_sip_h_P-Phy-Info">>, Props)}
     ,{<<"User-Agent">>, props:get_value(<<"variable_sip_user_agent">>, Props)}    
     ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
     ,{<<"SIP-Request-Host">>, props:get_value(<<"variable_sip_req_host">>, Props)}
     ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
     ,{<<"Switch-Hostname">>, props:get_value(<<"FreeSWITCH-Hostname">>, Props)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(route_req_ccvs(FetchId, Props))}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ]).

-spec route_req_ccvs(ne_binary(), wh_proplist()) -> wh_proplist().
route_req_ccvs(FetchId, Props) ->
    props:filter_undefined(
      [{<<"Fetch-ID">>, FetchId}
       | ecallmgr_util:custom_channel_vars(Props)
      ]
     ).


