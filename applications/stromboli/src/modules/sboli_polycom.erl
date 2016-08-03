%%%-------------------------------------------------------------------
%%% @author Ivan Romanyuk
%%% @copyright (C) 2016, <SIPLABS>
%%% @doc
%%%
%%% @end
%%% Created : 12. May 2016 13:13
%%%-------------------------------------------------------------------
-module(sboli_polycom).
-author("Ivan Romanyuk").

%% API
-export([try_get_mac/1
         ,try_get_dtl_file/1
         ,get_config_file/2
        ]).

-include("../stromboli.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(XML_PROLOG, <<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>">>).
-define(XML_POLYCOM_ROOT_TAG, 'polycomConfig').
-define(XML_POLYCOM_ROOT_PARAMS, [{'xmlns:xsi', <<"http://www.w3.org/2001/XMLSchema-instance">>}
                                  ,{'xsi:noNamespaceSchemaLocation' ,<<"polycomConfig.xsd">>}
                                 ]).

-spec try_get_mac(binary()) -> {'ok', stromboli:mac()} | {'error', 'not_found'}.
try_get_mac(FileName) ->
    case FileName of
        <<Mac:12/binary, ".cfg">> -> {'ok', Mac};
        <<Mac:12/binary, "-sip-basic.cfg">> -> {'ok', Mac};
        <<Mac:12/binary, "-reg-basic.cfg">> -> {'ok', Mac};
        <<Mac:12/binary, "-site.cfg">> -> {'ok', Mac};
%%        <<Mac:12/binary, "-directory.xml">> -> {'ok', Mac};
%%        <<Mac:12/binary, "-data_test.cfg">> -> {'ok', Mac};
%%        <<Mac:12/binary, "-phone1.cfg">> -> {'ok', Mac};
        _ ->
            lager:warning([{trace, true}], "Mac for FileName ~s is not found:(!!", [FileName]),
            {'error', 'not_found'}
    end.

-spec try_get_dtl_file(binary()) -> {'ok', binary()} | {'error', 'not_found'}.
try_get_dtl_file(FileName) ->
    case FileName of
        <<_Mac:12/binary, ".cfg">> -> {'ok', <<"polycom_master.dtl">>};
        <<_Mac:12/binary, "-sip-basic.cfg">> -> {'ok', <<"sip_basic.dtl">>};
        <<_Mac:12/binary, "-reg-basic.cfg">> -> {'ok', <<"reg_basic.dtl">>};
%%        <<_Mac:12/binary, "-directory.xml">> -> {'ok', <<"polycom_phonebook.dtl">>};
%%        <<_Mac:12/binary, "-phone1.cfg">> -> {'ok', <<"polycom_voice.dtl">>};
        _ ->
            lager:warning([{trace, true}], "Mac for FileName ~s is not found:(!!", [FileName]),
            {'error', 'not_found'}
    end.

-spec get_config_file(binary(), wh_json:object()) -> binary().
get_config_file(FileName, ConfJson) ->
    ConfJson1 = get_keys_by_config_file_name(FileName, ConfJson),
    lager:debug([{trace, true}], "The config json is: ~p", [ConfJson1]),
    Simple = json_to_simple(ConfJson1),
    lager:debug([{trace, true}], "Simple is: ~p", [Simple]),
    Xml = xmerl:export_simple([{?XML_POLYCOM_ROOT_TAG, ?XML_POLYCOM_ROOT_PARAMS, Simple}]
                          ,xmerl_xml
                          ,[{prolog, ?XML_PROLOG}]
                         ),
    unicode:characters_to_binary(Xml).

-spec get_keys_by_config_file_name(binary(), wh_json:object()) -> wh_json:objects().
get_keys_by_config_file_name(FileName, ConfJson) ->
    KeyGroupName =  case FileName of
                       <<_Mac:12/binary, "-site.cfg">> -> <<"site">>
                    end,
    Keys = wh_json:get_keys(<<"config">>, ConfJson),
    SelectedKeys = [Key || Key <- Keys, is_keygroup_member(Key, KeyGroupName)],
    lager:debug([{trace, true}], "Selected keys ~p", [SelectedKeys]),
    select_keys(SelectedKeys, ConfJson, wh_json:new()).

-spec select_keys(binaries(), wh_json:object(), wh_json:object()) ->
                        wh_json:objects().
select_keys([Key|Keys], ConfJson, NewConfJson) ->
    JObj = wh_json:get_value([<<"config">>, Key], ConfJson),
    NewConfJson1 = wh_json:set_value(Key, JObj, NewConfJson),
    select_keys(Keys, ConfJson, NewConfJson1);
select_keys([], _ConfJson, NewConfJson) -> NewConfJson.

-spec is_keygroup_member(binary(), binary()) -> boolean().
is_keygroup_member(Key, KeyGroupName) ->
    KeyGroups = [{<<"site">>,[<<"lcl">>, <<"tcpIpApp">>, <<"dialplan">>, <<"httpd">>, <<"device">>, <<"param_1">>, <<"param_2">>]}],
    case proplists:get_value(KeyGroupName, KeyGroups) of
       'undefined' -> 'false';
        KeyGroup -> lists:member(Key, KeyGroup)
    end.

-spec json_to_simple(wh_json:objects()) -> proplist().
-spec json_to_simple(wh_json:objects(), wh_json:keys()) -> proplist().
json_to_simple(?JSON_WRAPPER(JObjs)) ->
    lager:debug([{trace, true}], "JObjs is: ~p", [JObjs]),
    [json_to_simple(JObj, []) || JObj <- JObjs].

json_to_simple({Key, ?JSON_WRAPPER(Values)}, Path) when is_list(Values) ->
    Path1 = [Key|Path],
    Attributes = [{path_to_key([remove_underscore(K)|Path1]), V}
                  || {K, V} <- Values, not wh_json:is_json_object(V)
                 ],
    Content = [json_to_simple(Value, Path1) || {_, V} = Value <- Values
                                               ,wh_json:is_json_object(V)
              ],
    case Attributes of
        [] -> {path_to_key(Path1), Content};
        _ -> {path_to_key(Path1), Attributes, Content}
    end.

-spec remove_underscore(binary()) -> binary().
remove_underscore(<<"_", Rest/binary>>) -> Rest;
remove_underscore(BinStr) when is_binary(BinStr) -> BinStr.

-spec path_to_key(binaries()) -> atom().
path_to_key(Path) ->
    Htap = lists:reverse(Path),
    binary_to_atom(wh_util:join_binary(Htap, <<".">>), 'utf8').
