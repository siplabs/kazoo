%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(doodle_handlers).

-export([handle_route_req/2
        ]).

-include("doodle.hrl").

-spec handle_route_req(wh_json:object(), wh_proplist()) -> any().
handle_route_req(JObj, Props) ->
    'true' = wapi_sms:message_v(JObj),
    lager:info("DOODLE ~p",[JObj]),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    lager:info("DOODLE ~p",[CCVs]),
    AccountId = wh_json:get_value(<<"Account-ID">>, CCVs),
    OwnerId = wh_json:get_value(<<"Owner-ID">>, CCVs),
    AuthType = wh_json:get_value(<<"Authorizing-Type">>, CCVs),
    AuthId = wh_json:get_value(<<"Authorizing-ID">>, CCVs),
    Body = wh_json:get_value(<<"Body">>, JObj),
    To = wh_json:get_value(<<"To">>, JObj),
    From = wh_json:get_value(<<"From">>, JObj),
    Request = wh_json:get_value(<<"Request">>, JObj),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    
    Doc = props:filter_undefined([
           {<<"pvt_type">>, <<"sms">>}
          ,{<<"account_id">>, AccountId}
          ,{<<"owner_id">>, OwnerId}
          ,{AuthType, AuthId}
          ,{<<"to">>, To}
          ,{<<"from">>, From}
          ,{<<"request">>, Request}
          ,{<<"body">>, Body}
          ,{<<"direction">>, <<"inbound">>}
          ,{<<"pvt_created">>, wh_util:current_tstamp()}
          ,{<<"status">>, <<"queued">>}
           ]),
        
    
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    AccountMODb = kazoo_modb:get_modb(AccountDb),
    lager:info("saving ~p into ~s",[Doc, AccountMODb]),
    {'ok', SMSDoc } = kazoo_modb:save_doc(AccountDb, wh_json:from_list(Doc)),
    
    Doc1 = [ 
            {<<"account_db">>, AccountDb} 
           ,{<<"sms_id">>, wh_json:get_value(<<"_id">>, SMSDoc)}
          ,{<<"retries">>, 20}
           ,{<<"pvt_job_status">>, <<"pending">>}            
           | Doc],
    
    {'ok', SMSQueueDoc } = couch_mgr:save_doc(?DOODLE_DB, wh_json:from_list(Doc1)),
  
    'ok'.

