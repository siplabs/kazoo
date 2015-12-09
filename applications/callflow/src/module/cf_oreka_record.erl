%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(cf_oreka_record).

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
    lager:debug("sending command"),
    MuxStreams = wh_util:to_binary(wh_json:get_value(<<"mux_streams">>, Data, 'true')),
    Headers = wh_json:get_value(<<"headers">>, Data, wh_json:new()),
    whapps_call_command:oreka_record(Call, wh_json:set_value(<<"mux_streams">>, MuxStreams, Headers)),
    %% Give control back to cf_exe process
    cf_exe:continue(Call).
