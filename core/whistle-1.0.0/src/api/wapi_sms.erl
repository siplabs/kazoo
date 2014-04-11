%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Routing requests, responses, and wins!
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_sms).

-include_lib("whistle/include/wh_api.hrl").

-export([ 
          message/1, message_v/1
         ,bind_q/2, unbind_q/2
         ,declare_exchanges/0
         ,publish_message/1, publish_message/2
         ,publish_forward/1, publish_forward/2
         ,get_auth_realm/1
         ,get_auth_user/1
        ]).

-define(EVENT_CATEGORY, <<"sms">>).

%% routing keys to use in the callmgr exchange
-define(KEY_MESSAGE_REQ, <<"message">>). %% corresponds to the route_req/1 api call
-define(MESSAGE_REQ_EVENT_NAME, <<"message">>).
-define(MESSAGE_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, ?MESSAGE_REQ_EVENT_NAME}
                          ]).


%% Route Requests
-define(MESSAGE_HEADERS, [<<"To">>, <<"From">>, <<"Request">>, <<"Call-ID">>
                         ,<<"Body">>
                           ]).
-define(OPTIONAL_MESSAGE_HEADERS, [<<"Geo-Location">>, <<"Orig-IP">>
                                  ,<<"Custom-Channel-Vars">>
                                  ,<<"From-Network-Addr">>
                                  ,<<"Switch-Hostname">>, <<"Switch-Nodename">>
                                  ,<<"SIP-Request-Host">>, <<"Access-Network-Info">>
                                  ,<<"Physical-Info">>, <<"User-Agent">>
                                  ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                  ,<<"Contact">>
                                  ,<<"Contact-IP">>, <<"Contact-Port">>, <<"Contact-Username">>
    
                                    ]).
-define(MESSAGE_TYPES, [{<<"To">>, fun is_binary/1}
                          ,{<<"From">>, fun is_binary/1}
                          ,{<<"Request">>, fun is_binary/1}
                          ,{<<"Call-ID">>, fun is_binary/1}
                          ,{<<"Event-Queue">>, fun is_binary/1}
                          ,{<<"Caller-ID-Name">>, fun is_binary/1}
                          ,{<<"Caller-ID-Number">>, fun is_binary/1}
                          ,{<<"Custom-Channel-Vars">>, fun wh_json:is_json_object/1}
                         ]).


-define(KEY_FORWARD_REQ, <<"forward">>). %% corresponds to the route_req/1 api call
-define(FORWARD_REQ_EVENT_NAME, <<"forward">>).

-define(FORWARD_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, ?FORWARD_REQ_EVENT_NAME}
                          ]).


-define(MESSAGE_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                        ,{<<"Event-Name">>, [?MESSAGE_REQ_EVENT_NAME , ?FORWARD_REQ_EVENT_NAME]}
                        ]).


-spec message(api_terms()) -> {'ok', iolist()} | {'error', string()}.
message(Prop) when is_list(Prop) ->
    case message_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?MESSAGE_HEADERS, ?OPTIONAL_MESSAGE_HEADERS);
        'false' -> {'error', "Proplist failed validation for route_message"}
    end;
message(JObj) -> message(wh_json:to_proplist(JObj)).

-spec message_v(api_terms()) -> boolean().
message_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MESSAGE_HEADERS, ?MESSAGE_VALUES, ?MESSAGE_TYPES);
message_v(JObj) -> message_v(wh_json:to_proplist(JObj)).




%%--------------------------------------------------------------------
%% @doc Bind AMQP Queue for routing requests
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    User = props:get_value('user', Props, <<"*">>),
    amqp_util:bind_q_to_callmgr(Queue, get_message_routing(?KEY_MESSAGE_REQ, Realm, User)),
    amqp_util:bind_q_to_callmgr(Queue, get_message_routing(?KEY_FORWARD_REQ, Realm, User)).

-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    User = props:get_value('user', Props, <<"*">>),
    amqp_util:unbind_q_from_callmgr(Queue, get_message_routing(?KEY_MESSAGE_REQ, Realm, User)),
    amqp_util:unbind_q_from_callmgr(Queue, get_message_routing(?KEY_FORWARD_REQ, Realm, User)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:callmgr_exchange().

-spec get_message_routing(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
get_message_routing(Type, Realm, User) ->
    list_to_binary([Type, ".", amqp_util:encode(Realm), ".", amqp_util:encode(User)]).

-spec get_message_routing(ne_binary(), api_terms()) -> ne_binary().
get_message_routing(Type, Api) ->
    {User, Realm} = get_auth_user_realm(Api),
    get_message_routing(Type, Realm, User).

-spec publish_message(api_terms()) -> 'ok'.
-spec publish_message(api_terms(), binary()) -> 'ok'.
publish_message(JObj) ->
    publish_message(JObj, ?DEFAULT_CONTENT_TYPE).
publish_message(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?MESSAGE_REQ_VALUES, fun message/1),
    amqp_util:callmgr_publish(Payload, ContentType, get_message_routing(?KEY_MESSAGE_REQ, Req)).


-spec publish_forward(api_terms()) -> 'ok'.
-spec publish_forward(api_terms(), binary()) -> 'ok'.
publish_forward(JObj) ->
    publish_forward(JObj, ?DEFAULT_CONTENT_TYPE).
publish_forward(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?FORWARD_REQ_VALUES, fun message/1),
    amqp_util:callmgr_publish(Payload, ContentType, get_message_routing(?KEY_FORWARD_REQ, Req)).




%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%-----------------------------------------------------------------------------
-spec get_auth_realm(api_terms()) -> ne_binary().
get_auth_realm(ApiProp) when is_list(ApiProp) ->
    [_ReqUser, ReqDomain] = binary:split(props:get_value(<<"From">>, ApiProp), <<"@">>),
    ReqDomain;
get_auth_realm(ApiJObj) ->
    [_ReqUser, ReqDomain] = binary:split(wh_json:get_value(<<"From">>, ApiJObj), <<"@">>),
    ReqDomain.

-spec get_auth_user(api_terms()) -> ne_binary().
get_auth_user(ApiProp) when is_list(ApiProp) ->
    [ReqUser, _ReqDomain] = binary:split(props:get_value(<<"From">>, ApiProp), <<"@">>),
    ReqUser;
get_auth_user(ApiJObj) ->
    [ReqUser, _ReqDomain] = binary:split(wh_json:get_value(<<"From">>, ApiJObj), <<"@">>),
    ReqUser.

-spec get_auth_user_realm(api_terms()) -> {ne_binary(), ne_binary()}.
get_auth_user_realm(ApiProp) when is_list(ApiProp) ->
    [ReqUser, ReqDomain] = binary:split(props:get_value(<<"From">>, ApiProp), <<"@">>),
    {ReqUser, ReqDomain};
get_auth_user_realm(ApiJObj) ->
    [ReqUser, ReqDomain] = binary:split(wh_json:get_value(<<"From">>, ApiJObj), <<"@">>),
    {ReqUser, ReqDomain}.
