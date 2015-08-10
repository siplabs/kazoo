%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cccp_auth).

-include("cccp.hrl").

%% API
-export([authorize/2
         ,account_id/1
         ,outbound_cid/1
         ,auth_doc_id/1
         ,pin/1
        ]).

-export_type([cccp_auth/0
              ,cccp_auth_ret/0
             ]).

-define(DEFAULT_CALLER_ID, whapps_config:get(?CCCP_CONFIG_CAT, <<"default_caller_id_number">>, <<"00000000000">>)).

-record(auth, {account_id :: ne_binary()
               ,outbound_cid = ?DEFAULT_CALLER_ID :: ne_binary()
               ,auth_doc_id :: ne_binary()
               ,pin :: api_binary()
              }).

-type cccp_auth() :: #auth{}.
-type cccp_auth_ret() :: {'ok', cccp_auth()} | {'error', any()}.

-spec account_id(cccp_auth()) -> ne_binary().
account_id(#auth{account_id = AccountId}) ->
    AccountId.

-spec account_db(cccp_auth()) -> ne_binary().
account_db(#auth{account_id = AccountId}) ->
    wh_util:format_account_id(AccountId, 'encoded').

-spec outbound_cid(cccp_auth()) -> ne_binary().
outbound_cid(#auth{outbound_cid = CID}) ->
    CID.

-spec auth_doc_id(cccp_auth()) -> ne_binary().
auth_doc_id(#auth{auth_doc_id = AuthId}) ->
    AuthId.

-spec pin(cccp_auth()) -> api_binary().
pin(#auth{pin = Pin}) ->
    Pin.

-spec authorize(ne_binary(), ne_binary()) -> cccp_auth_ret().
authorize(Value, View) ->
    ViewOptions = [{'key', Value}],
    case couch_mgr:get_results(?KZ_CCCPS_DB, View, ViewOptions) of
        {'ok',[]} ->
            lager:info("Auth by ~p failed for: ~p. No such value in Db.", [Value, View]),
            {'error', 'empty'};
        {'ok', [JObj]} ->
            maybe_legalize_outbound_cid(#auth{account_id = wh_json:get_value([<<"value">>,<<"account_id">>], JObj)
                                             ,auth_doc_id = wh_json:get_value([<<"value">>,<<"id">>], JObj)
                                             ,pin = wh_json:get_value([<<"value">>,<<"pin">>], JObj)
                                             }
                                        ,wh_json:get_value([<<"value">>,<<"outbound_cid">>], JObj)
                                       );
        {'ok', [_|_]} ->
            lager:error("Auth by ~p failed for: ~p. Too many results.", [Value, View]),
            {'error', 'not_unique'};
        {'error', R} ->
            lager:info("Auth failed for ~p. Error occurred: ~p.", [Value, R]),
            {'error', R}
    end.

-spec maybe_legalize_outbound_cid(cccp_auth(), api_binary()) -> cccp_auth_ret().
maybe_legalize_outbound_cid(Auth, 'undefined') ->
    {'ok', Auth};
maybe_legalize_outbound_cid(Auth, CID) ->
    case whapps_config:get_is_true(?CCCP_CONFIG_CAT, <<"ensure_valid_caller_id">>, 'true') of
        'true' -> ensure_valid_caller_id(Auth, CID);
        'false' -> {'ok', Auth}
    end.

-spec ensure_valid_caller_id(cccp_auth(), ne_binary()) -> cccp_auth_ret().
ensure_valid_caller_id(Auth, OutboundCID) ->
    {'ok', AccountPhoneNumbersList} = couch_mgr:open_cache_doc(account_db(Auth)
                                                               ,?WNM_PHONE_NUMBER_DOC
                                                              ),
    case lists:member(wnm_util:normalize_number(OutboundCID)
                      ,wh_json:get_keys(AccountPhoneNumbersList)
                     )
    of
        'true' ->
            {'ok', Auth#auth{outbound_cid = OutboundCID}};
        'false' ->
            lager:debug("OutboundCID ~p is out of account's list; changing to application's default: ~p"
                        ,[OutboundCID
                          ,outbound_cid(Auth)
                         ]),
            {'ok', Auth}
    end.
