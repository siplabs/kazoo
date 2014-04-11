%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Look up IP for authorization/replaying of route_req
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(imsi_handle_authz).

-export([init/0, handle_req/2]).

-include("imsi.hrl").

init() -> 'ok'.


-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    lager:info("AUTHZ - ~p",[JObj]),
    'true' = wapi_trusted:authz_req_v(JObj),
    CCVs = case wh_json:get_ne_value( [<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
               'undefined' -> maybe_lookup_user(JObj);
               _AccountId -> []           
           end,
    send_response(CCVs, JObj).

                   
                   
    
maybe_lookup_user(JObj) ->
    %Realm = wh_json:get_value(<<"SIP-Request-Host">>, JObj),
    [Username, Realm] = binary:split(wh_json:get_value(<<"From">>, JObj), <<"@">>),
    ViewOptions = [{'key', [Realm, wh_util:to_lower_binary( Username )]}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
      {'ok', [Doc|_]} -> get_ccvs(JObj, Doc);
      _Else ->
          maybe_lookup_imsi(wh_util:to_upper_binary(Username), JObj)
    end.

maybe_lookup_imsi(<<"IMSI", _/binary>>=Username, JObj) ->
    ViewOptions = [{'key', Username}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/imsi">>, ViewOptions) of
      {'ok', [Doc|_]} -> get_ccvs(JObj, Doc);
      _Else -> []
    end;
maybe_lookup_imsi(Username, JObj) ->
    [].

-spec get_ccvs(wh_json:object(), wh_json:object()) -> any().
get_ccvs(JObj, Doc) ->
    AccountID = wh_json:get_value([<<"value">>,  <<"account_id">>], Doc),
    OwnerID = wh_json:get_value([<<"value">>, <<"owner_id">>], Doc),
    AuthType = wh_json:get_value([<<"value">>, <<"authorizing_type">>], Doc, <<"anonymous">>),
    Props = props:filter_undefined(
               [{<<"Account-ID">>, AccountID}
               ,{<<"Owner-ID">>, OwnerID}
               ,{<<"Authorizing-ID">>, wh_json:get_value(<<"id">>, Doc)}
               ,{<<"Authorizing-Type">>, AuthType}
               ]),
    Props.

send_response(Props, JObj) ->   
    CCVs = wh_json:from_list(props:filter_undefined(Props)),
    Category = wh_json:get_value(<<"Event-Category">>, JObj),
    Resp =   [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
             ,{<<"Custom-Channel-Vars">>, CCVs}             
              | wh_api:default_headers(<<"trusted">>, <<"authz_resp">>, ?APP_NAME, ?APP_VERSION)
             ],
    wapi_trusted:publish_authz_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).    
