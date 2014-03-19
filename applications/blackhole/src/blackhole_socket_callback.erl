%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_socket_callback).

-include("blackhole.hrl").

-export([open/3
        ,recv/4
        ,close/3
        ]).

open(SessionPid, SessionId, _Opts) ->
    lager:debug("opening socket ~p", [SessionId]),
    {'ok', bh_context:new(SessionPid, SessionId)}.
 
recv(SessionPid, SessionId, {message, <<>>, Message}, State) ->
    lager:debug("received message ~p on socket ~p", [Message, SessionId]),
    blackhole_resource:handle_message(Message, SessionId, SessionPid),
    {'ok', State};

recv(_SessionPid, _SessionId, {'event', _Ignore, <<"subscribe">>, SubscriptionJObj}, Context) ->
    Context1 = bh_context:from_subscription(Context, SubscriptionJObj),
    case blackhole_util:is_authorized(Context1) of
        'true' ->
            Binding = bh_context:binding(Context1),
            lager:debug("going to bind with binding: ~s", [Binding]),
            case blackhole_util:get_callback_module(Binding) of
                'undefined' -> blackhole_util:respond_with_error(Context1);
                M ->
                    maybe_add_binding_to_listener(M, Binding, Context1),
                    blackhole_bindings:bind(Binding, M, 'handle_event', Context1)
            end;
        'false' ->
            blackhole_util:respond_with_authn_failure(Context1)
    end,
    {'ok', Context1};

recv(_SessionPid, _SessionId, {'event', _Ignore, _Event, _Data}, Context) ->
    lager:debug("received event: ~p on socket ~p with data payload", [_Event, _SessionId]),
    {'ok', Context};
recv(_SessionPid, SessionId, Message, Context) ->
    lager:info("receive unknown message ~p on socket ~p", [Message, SessionId]),
    {'ok', Context}.

maybe_add_binding_to_listener(Module, Binding, Context) ->
    try Module:add_amqp_binding(Binding, Context) of
        _ -> 'ok'
    catch
        _:_ -> 'ok'
    end.

close(_SessionPid, SessionId, _State) ->
    lager:debug("closing socket ~p", [SessionId]),
    %%TODO - Remmove ourselves from binding server
    'ok'.