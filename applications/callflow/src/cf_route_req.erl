%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% handler for route requests, responds if callflows match
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_route_req).

-export([handle_req/2]).

-include("callflow.hrl").

-define(RESOURCE_TYPES_HANDLED, [<<"audio">>, <<"video">>]).

-define(DEFAULT_METAFLOWS(AccountId)
        ,whapps_account_config:get(AccountId, <<"metaflows">>, <<"default_metaflow">>, 'false')
       ).

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3000).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
-define(ROUTE_WIN_TIMEOUT, whapps_config:get_integer(?CF_CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),
    Call = maybe_call_rerouted(whapps_call:from_route_req(JObj)),
    case is_binary(whapps_call:account_id(Call))
        andalso callflow_should_respond(Call)
        andalso callflow_resource_allowed(Call)
    of
        'true' ->
            lager:info("received a request asking if callflows can route this call"),
            AllowNoMatch = allow_no_match(Call),
            case cf_util:lookup_callflow(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {'ok', Flow, NoMatch} when (not NoMatch) orelse AllowNoMatch ->
                    maybe_prepend_preflow(JObj, Props, Call, Flow, NoMatch);
                {'ok', _, 'true'} ->
                    lager:info("only available callflow is a nomatch for a unauthorized call", []);
                {'error', R} ->
                    lager:info("unable to find callflow ~p", [R])
            end;
        'false' ->
            'ok'
    end.

-spec maybe_prepend_preflow(wh_json:object(), wh_proplist()
                            ,whapps_call:call(), wh_json:object()
                            ,boolean()
                           ) -> whapps_call:call().
maybe_prepend_preflow(JObj, Props, Call, Flow, NoMatch) ->
    AccountId = whapps_call:account_id(Call),
    case kz_account:fetch(AccountId) of
        {'error', _E} ->
            lager:warning("could not open account doc ~s : ~p", [AccountId, _E]),
            maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch);
        {'ok', Doc} ->
            case wh_json:get_ne_value([<<"preflow">>, <<"always">>], Doc) of
                'undefined' ->
                    lager:debug("ignore preflow, not set"),
                    maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch);
                PreflowId ->
                    NewFlow = prepend_preflow(AccountId, PreflowId, Flow),
                    maybe_reply_to_req(JObj, Props, Call, NewFlow, NoMatch)
            end
    end.

-spec prepend_preflow(ne_binary(), ne_binary(), wh_json:object()) ->
                             wh_json:object().
prepend_preflow(AccountId, PreflowId, Flow) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, PreflowId) of
        {'error', _E} ->
            lager:warning("could not open ~s in ~s : ~p", [PreflowId, AccountDb, _E]),
            Flow;
        {'ok', Doc} ->
            Children = wh_json:from_list([{<<"_">>, wh_json:get_value(<<"flow">>, Flow)}]),
            Preflow = wh_json:set_value(<<"children">>
                                        ,Children
                                        ,wh_json:get_value(<<"flow">>, Doc)
                                       ),
            wh_json:set_value(<<"flow">>, Preflow, Flow)
    end.

-spec maybe_reply_to_req(wh_json:object(), wh_proplist()
                         ,whapps_call:call(), wh_json:object(), boolean()) -> whapps_call:call().
maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch) ->
    lager:info("callflow ~s in ~s satisfies request", [wh_doc:id(Flow)
                                                       ,whapps_call:account_id(Call)
                                                      ]),
    {Name, Cost} = bucket_info(Call, Flow),
    case kz_buckets:consume_tokens(?APP_NAME, Name, Cost) of
        'false' ->
            lager:debug("bucket ~s doesn't have enough tokens(~b needed) for this call", [Name, Cost]);
        'true' ->
            ControllerQ = props:get_value('queue', Props),
            NewCall = update_call(Flow, NoMatch, ControllerQ, Call),
            send_route_response(Flow, JObj, NewCall)
    end.

-spec bucket_info(whapps_call:call(), wh_json:object()) ->
                         {ne_binary(), pos_integer()}.
bucket_info(Call, Flow) ->
    case wh_json:get_value(<<"pvt_bucket_name">>, Flow) of
        'undefined' -> {bucket_name_from_call(Call, Flow), bucket_cost(Flow)};
        Name -> {Name, bucket_cost(Flow)}
    end.

-spec bucket_name_from_call(whapps_call:call(), wh_json:object()) -> ne_binary().
bucket_name_from_call(Call, Flow) ->
    <<(whapps_call:account_id(Call))/binary, ":", (wh_doc:id(Flow))/binary>>.

-spec bucket_cost(wh_json:object()) -> pos_integer().
bucket_cost(Flow) ->
    Min = whapps_config:get_integer(?CF_CONFIG_CAT, <<"min_bucket_cost">>, 5),
    case wh_json:get_integer_value(<<"pvt_bucket_cost">>, Flow) of
        'undefined' -> Min;
        N when N < Min -> Min;
        N -> N
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% Should this call be able to use outbound resources, the exact opposite
%% exists in the handoff module.  When updating this one make sure to sync
%% the change with that module
%% @end
%%-----------------------------------------------------------------------------
-spec allow_no_match(whapps_call:call()) -> boolean().
allow_no_match(Call) ->
    whapps_call:custom_channel_var(<<"Referred-By">>, Call) =/= 'undefined'
        orelse allow_no_match_type(Call).

-spec allow_no_match_type(whapps_call:call()) -> boolean().
allow_no_match_type(Call) ->
    case whapps_call:authorizing_type(Call) of
        'undefined' -> 'false';
        <<"resource">> -> 'false';
        <<"sys_info">> -> 'false';
        _ -> 'true'
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% determine if callflows should respond to a route request
%% @end
%%-----------------------------------------------------------------------------
-spec callflow_should_respond(whapps_call:call()) -> boolean().
callflow_should_respond(Call) ->
    case whapps_call:authorizing_type(Call) of
        <<"account">> -> 'true';
        <<"user">> -> 'true';
        <<"device">> -> 'true';
        <<"callforward">> -> 'true';
        <<"clicktocall">> -> 'true';
        <<"resource">> -> 'true';
        <<"sys_info">> ->
            timer:sleep(500),
            Number = whapps_call:request_user(Call),
            (not wnm_util:is_reconcilable(Number));
        'undefined' -> 'true';
        _Else -> 'false'
    end.

-spec callflow_resource_allowed(whapps_call:call()) -> boolean().
callflow_resource_allowed(Call) ->
    is_resource_allowed(whapps_call:resource_type(Call)).

-spec is_resource_allowed(api_binary()) -> boolean().
is_resource_allowed('undefined') -> 'true';
is_resource_allowed(ResourceType) ->
    lists:member(ResourceType, ?RESOURCE_TYPES_HANDLED).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec send_route_response(wh_json:object(), wh_json:object(), whapps_call:call()) -> 'ok'.
send_route_response(Flow, JObj, Call) ->
    lager:info("callflows knows how to route the call! sending park response"),
    AccountId = whapps_call:account_id(Call),
    Resp = props:filter_undefined(
             [{?KEY_MSG_ID, wh_api:msg_id(JObj)}
              ,{?KEY_MSG_REPLY_ID, whapps_call:call_id_direct(Call)}
              ,{<<"Routes">>, []}
              ,{<<"Method">>, <<"park">>}
              ,{<<"Transfer-Media">>, get_transfer_media(Flow, JObj)}
              ,{<<"Ringback-Media">>, get_ringback_media(Flow, JObj)}
              ,{<<"Pre-Park">>, pre_park_action(Call)}
              ,{<<"From-Realm">>, wh_util:get_account_realm(AccountId)}
              ,{<<"Custom-Channel-Vars">>, whapps_call:custom_channel_vars(Call)}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    ServerId = wh_api:server_id(JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    case wh_amqp_worker:call(Resp
                             ,Publisher
                             ,fun wapi_route:win_v/1
                             ,?ROUTE_WIN_TIMEOUT
                            )
    of
        {'ok', RouteWin} ->
            lager:info("callflow has received a route win, taking control of the call"),
            cf_route_win:execute_callflow(RouteWin, whapps_call:from_route_win(RouteWin, Call));
        {'error', _E} ->
            lager:info("callflow didn't received a route win, exiting : ~p", [_E])
    end.

-spec get_transfer_media(wh_json:object(), wh_json:object()) -> api_binary().
get_transfer_media(Flow, JObj) ->
    case wh_json:get_value([<<"ringback">>, <<"transfer">>], Flow) of
        'undefined' ->
            wh_json:get_value(<<"Transfer-Media">>, JObj);
        MediaId -> MediaId
    end.

-spec get_ringback_media(wh_json:object(), wh_json:object()) -> api_binary().
get_ringback_media(Flow, JObj) ->
    case wh_json:get_value([<<"ringback">>, <<"early">>], Flow) of
        'undefined' ->
            wh_json:get_value(<<"Ringback-Media">>, JObj);
        MediaId -> MediaId
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec pre_park_action(whapps_call:call()) -> ne_binary().
pre_park_action(Call) ->
    case whapps_config:get_is_true(<<"callflow">>, <<"ring_ready_offnet">>, 'true')
        andalso whapps_call:inception(Call) =/= 'undefined'
        andalso whapps_call:authorizing_type(Call) =:= 'undefined'
    of
        'false' -> <<"none">>;
        'true' -> <<"ring_ready">>
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec update_call(wh_json:object(), boolean(), ne_binary(), whapps_call:call()) ->
                         whapps_call:call().
update_call(Flow, NoMatch, ControllerQ, Call) ->
    Props = [{'cf_flow_id', wh_doc:id(Flow)}
             ,{'cf_flow', wh_json:get_value(<<"flow">>, Flow)}
             ,{'cf_capture_group', wh_json:get_ne_value(<<"capture_group">>, Flow)}
             ,{'cf_no_match', NoMatch}
             ,{'cf_metaflow', wh_json:get_value(<<"metaflows">>, Flow, ?DEFAULT_METAFLOWS(whapps_call:account_id(Call)))}
            ],

    Updaters = [{fun whapps_call:kvs_store_proplist/2, Props}
                ,{fun whapps_call:set_controller_queue/2, ControllerQ}
                ,{fun whapps_call:set_application_name/2, ?APP_NAME}
                ,{fun whapps_call:set_application_version/2, ?APP_VERSION}
               ],
    whapps_call:exec(Updaters, Call).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec maybe_call_rerouted(whapps_call:call()) -> whapps_call:call().
maybe_call_rerouted(Call) ->
    case get_rerouted_by(Call) of
        'undefined' -> Call;
        ReroutedBy ->
            ReOptions = [{'capture', [1], 'binary'}],
            case catch(re:run(ReroutedBy, <<".*sip:(.*)@.*">>, ReOptions)) of
                {'match', [Match]} -> fix_rerouted_call(Match, Call);
                _ -> Call
            end
    end.

-spec fix_rerouted_call(api_binary(), whapps_call:call()) -> whapps_call:call().
fix_rerouted_call(ReroutedBy, Call) ->
    [Username|_] = binary:split(ReroutedBy, <<"@">>),
    case cf_util:endpoint_id_by_sip_username(whapps_call:account_db(Call), Username) of
        {'ok', EndpointId} ->
            maybe_set_reroute_owner(
              whapps_call:set_authorization(<<"device">>, EndpointId, Call)
             );
        {'error', 'not_found'} ->
            Keys = [<<"Account-ID">>
                    ,<<"Owner-ID">>
                    ,<<"Authorizing-Type">>
                    ,<<"Authorizing-ID">>
                   ],
            whapps_call:remove_custom_channel_vars(Keys, Call)
    end.

-spec maybe_set_reroute_owner(whapps_call:call()) -> whapps_call:call().
maybe_set_reroute_owner(Call) ->
    case cf_attributes:owner_id(Call) of
        'undefined' ->
            whapps_call:remove_custom_channel_vars([<<"Owner-ID">>], Call);
        OwnerId ->
            whapps_call:set_owner_id(OwnerId, Call)
    end.

-spec get_rerouted_by(whapps_call:call()) -> api_binary().
get_rerouted_by(Call) ->
    case whapps_call:custom_channel_var(<<"Redirected-By">>, Call) of
        'undefined' -> whapps_call:custom_channel_var(<<"Referred-By">>, Call);
        RedirectedBy -> RedirectedBy
    end.
