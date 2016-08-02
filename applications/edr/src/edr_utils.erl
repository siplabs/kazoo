%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Utils for backends
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(edr_utils).

-include("edr.hrl").

-export([distribute_event/2
         ,registred_backends/0
         ,register_backend/5
         ,delete_backend/1
         ,enable_backend/1
         ,disable_backend/1
        ]).

-spec register_backend(ne_binary(), ne_binary(), wh_json:object(), wh_json:object(), boolean())-> 'ok' | {'error', 'already_registred'}.
register_backend(Name, Type, Tags, Opts, IsEnable)->
    JBackends = whapps_config:get(<<"edr">>, <<"backends">>, wh_json:new()),
    case wh_json:get_value(Name, JBackends) of
        'undefined' ->
            Backend = wh_json:from_list([{<<"Name">>, Name}
                                         ,{<<"Options">>, Opts}
                                         ,{<<"Tags">>, Tags}
                                         ,{<<"Type">>, Type}
                                         ,{<<"Enabled">>, IsEnable}
                                        ]),
            NewBackends = wh_json:set_value(Name, Backend, JBackends),
            {'ok', _} = whapps_config:set(<<"edr">>, <<"backends">>, NewBackends),
            'ok';
        _V -> {'error', 'already_registred'}
    end.

-spec delete_backend(ne_binary())-> 'ok'.
delete_backend(Name)->
    Backends = whapps_config:get(<<"edr">>, <<"backends">>, wh_json:new()),
    NewBackends = wh_json:delete_key(Name, Backends),
    {'ok', _} = whapps_config:set(<<"edr">>, <<"backends">>, NewBackends),
    'ok'.

-spec enable_backend(ne_binary())-> 'ok' | {'error', 'not_registred'}.
enable_backend(Name)->
    Backends = whapps_config:get(<<"edr">>, <<"backends">>, wh_json:new()),
    case wh_json:get_value(Name, Backends) of
        'undefined' -> {'error', 'not_registred'};
        Backend ->
            NewBackend = wh_json:set_value(<<"Enabled">>, 'true', Backend),
            NewBackends = wh_json:set_value(Name, NewBackend, Backends),
            {'ok', _} = whapps_config:set(<<"edr">>, <<"backends">>, NewBackends),
            'ok'
    end.

-spec disable_backend(ne_binary())-> 'ok'.
disable_backend(Name)->
    Backends = whapps_config:get(<<"edr">>, <<"backends">>, wh_json:new()),
    case wh_json:get_value(Name, Backends) of
        'undefined' -> {'error', 'not_registred'};
        Backend ->
            NewBackend = wh_json:set_value(<<"Enabled">>, 'false', Backend),
            NewBackends = wh_json:set_value(Name, NewBackend, Backends),
            {'ok', _} = whapps_config:set(<<"edr">>, <<"backends">>, NewBackends),
            'ok'
    end.


-spec registred_backends()-> wh_json:object().
registred_backends()->
    whapps_config:get(<<"edr">>, <<"backends">>, wh_json:new()).


-spec distribute_event(non_neg_integer(), wh_json:object())-> 'ok'.
distribute_event(Timestamp, EventData)->
    lists:foreach(fun ({_,Pid,_})->
                          gen_backend:push(Pid, Timestamp, EventData)
                  end, edr_backend_sup:get_running_backends()).
