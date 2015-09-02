%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% "data":{
%%%     "source_type":"text" | "variable" | "kvs",
%%%     "source": "text_to_say_or_var_name"
%%%     "say_type":{{say_type}}
%%% }
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_say).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Text = get_text(wh_json:get_value(<<"source_type">>, Data)
                    ,wh_json:get_value(<<"source">>, Data)
                    ,Call
                   ),
    SayType = wh_json:get_value(<<"say_type">>, Data),
    lager:info("saying ~s", [Text]),
    case is_binary(Text) andalso byte_size(Text) > 0 of
        'true' -> whapps_call_command:b_say(Text, SayType, Call);
        'false' -> 'ok'
    end,
    cf_exe:continue(Call).

-spec get_text(api_binary(), api_binary(), whapps_call:call()) -> api_binary().
get_text('undefined', Source, Call) ->
    get_text(<<"text">>, Source, Call);
get_text(<<"text">>, Source, _Call) ->
    Source;
get_text(<<"variable">>, Variable, Call) ->
    whapps_call:custom_channel_var(Variable, Call);
get_text(<<"kvs">>, Var, Call) ->
    whapps_call:kvs_fetch(Var, Call).
