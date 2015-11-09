%%%-------------------------------------------------------------------
%%% @copyright (C) 2011 VoIP INC
%%% @doc
%%% FS passthrough API
%%% @end
%%%-------------------------------------------------------------------
-module(wapi_leader).

-export([whoami/0, whoami/1]).
-export([queue/0, queue/1, queue/2]).
-export([route/0, route/1, route/2]).
-export([req/1, req_v/1]).
-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).
-export([publish_req/2, publish_req/3]).

-include_lib("whistle/include/wh_api.hrl").

-define(LEADER_REQ_HEADERS, []).
-define(LEADER_REQ_VALUES, []).
-define(LEADER_REQ_TYPES, []).
-define(OPTIONAL_LEADER_REQ_HEADERS, [<<"Message">>]).

whoami() ->
    whoami(self()).

whoami(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, 'registered_name') of
        {'registered_name', Name} -> {Name, node()};
        _ -> Pid
    end.

queue() ->
    queue(self()).

queue(Pid) when is_pid(Pid) ->
    case whoami(Pid) of
        Pid -> exit(<<"not registered">>);
        Name ->
            queue(Name)
    end;
queue(Name) when is_atom(Name) ->
    queue(Name, node());
queue({Name, Node}) ->
    queue(Name, Node).

queue(Name, Node) ->
    iolist_to_binary(io_lib:format("~s-~s", [Node, Name])).

route() ->
    route(self()).

route(Pid) when is_pid(Pid) ->
    {'registered_name', Name} = erlang:process_info(Pid, 'registered_name'),
    route(Name);
route(Name) when is_atom(Name) ->
    route(Name, node()).

route(Name, Node) ->
    iolist_to_binary(io_lib:format("~s.~s", [Name, Node])).

-spec req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
req(Prop) when is_list(Prop) ->
    case req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?LEADER_REQ_HEADERS, ?OPTIONAL_LEADER_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for fs_req"}
    end;
req(JObj) ->
    req(wh_json:to_proplist(JObj)).

-spec req_v(api_terms()) -> boolean().
req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?LEADER_REQ_HEADERS, ?LEADER_REQ_VALUES, ?LEADER_REQ_TYPES);
req_v(JObj) ->
    req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:leader_exchange().

-spec publish_req(ne_binary(), api_terms()) -> 'ok'.
-spec publish_req(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_req(Routing, JObj) ->
    publish_req(Routing, JObj, ?DEFAULT_CONTENT_TYPE).
publish_req(Routing, Req, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Req, ?LEADER_REQ_VALUES, fun ?MODULE:req/1),
    amqp_util:leader_publish(Routing, Payload, ContentType).

-spec bind_q(ne_binary(), ne_binaries()) -> 'ok'.
bind_q(Queue, [{'name', Name}]) ->
    Node = node(),
    ProcessQ = iolist_to_binary(io_lib:format("~s.~s", [Name, Node])),
    BroadcastQ = iolist_to_binary(io_lib:format("~s.broadcast", [Name])),
    ['ok' = amqp_util:bind_q_to_leader(Queue, Bind) || Bind <- [ProcessQ, BroadcastQ]].

unbind_q(Queue, _) ->
    'ok' = amqp_util:unbind_q_from_leader(Queue, <<"process">>).
