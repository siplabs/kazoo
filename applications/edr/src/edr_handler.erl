%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(edr_handler).

-include("edr.hrl").

-export([handle_req/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props)->
    _Timestamp = wh_json:get_value(<<"Timestamp">>, JObj),
    _Tags = wh_json:get_value(<<"Tags">>, JObj),
    %% TODO: distribute_event(Timestamp, Tags)
    'ok'.
