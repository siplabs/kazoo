%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Transfers caller to the extension extracted in the regex
%%% Data = {
%%%   "target":"1000" // extension/DID to transfer to, optional
%%% }
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(konami_blind_transfer).

%% API
-export([handle/2]).

-spec handle(wh_json:object(), whapps_call:call()) -> no_return().
handle(Data, Call) ->
    Transferor = wh_json:get_value(<<"dtmf_leg">>, Data),
    Transferee =
        case whapps_call:call_id(Call) of
            Transferor -> whapps_call:other_leg_call_id(Call);
            CallId -> CallId
        end,
    Extension = case wh_json:get_first_defined([<<"captures">>, <<"target">>], Data) of
                    [Ext | _] -> Ext;
                    Else -> Else
                end,
    CCVs = wh_json:from_list([{<<"Authorizing-Type">>, <<"callforward">>}
                              ,{<<"Channel-Authorized">>, <<"true">>}
                             ]),
    whapps_call_command:transfer(Extension, Transferee, CCVs, Call),
    {'continue', Call}.
