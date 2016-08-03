%%%-------------------------------------------------------------------
%%% @author Ivan Romanyuk
%%% @copyright (C) 2016, <SIPLABS>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2016 11:22
%%%-------------------------------------------------------------------
-module(mc_json).
-author("Ivan Romanyuk").

%% API
-export([merge_recursive/1]).

-include_lib("whistle/include/wh_types.hrl").

-define(JSON_WRAPPER(Proplist), {Proplist}).

-spec merge_recursive([{wh_json:object(), wh_json:object()}]) ->
                         {wh_json:object(), wh_json:object()}.
-spec merge_recursive(wh_json:object()
                      ,wh_json:object() | wh_json:json_term()
                      ,wh_json:object()
                      ,wh_json:keys()) -> wh_json:object().
merge_recursive([]) -> {wh_json:new(), wh_json:new()};
merge_recursive([{JObj, Lock}|Rest]) ->
    lists:foldl(fun({JObj2, Lock2}, {JObjAcc, LockAcc}) ->
                        JObjAcc1 = merge_recursive(JObjAcc, JObj2, LockAcc, []),
                        LockAcc1 = merge_locks(LockAcc, Lock2),
                        {JObjAcc1, LockAcc1}
                end, {JObj , Lock}, Rest).

merge_recursive(?JSON_WRAPPER(_)=JObj1, ?JSON_WRAPPER(_)=JObj2, LockJobj, Keys) ->
    wh_json:foldl(fun(Key2, Value2, JObj1Acc) ->
        merge_recursive(JObj1Acc, Value2, LockJobj, [Key2|Keys])
                  end, JObj1, JObj2);
merge_recursive(?JSON_WRAPPER(_)=JObj1, Value, LockJobj, Keys) ->
    Syek = lists:reverse(Keys),
    case check_if_can_modify(Keys, LockJobj) of
        'false' -> JObj1;
        'true' -> wh_json:set_value(Syek, Value, JObj1)
    end.

-spec merge_locks(wh_json:object(), wh_json:object()) ->
                    wh_json:object().
merge_locks(JObj1, JObj2) ->
    merge_recursive(JObj1, JObj2, JObj1, []).

-spec check_if_can_modify(wh_json:key(), wh_json:objects()) -> boolean().
check_if_can_modify([], _LockJobj) -> 'true';
check_if_can_modify(Path, LockJobj) ->
    Htap = lists:reverse(Path),
    case wh_json:get_value(Htap, LockJobj) of
        <<"locked">> -> 'false';
        _ ->
            check_if_can_modify(tl(Path), LockJobj)
    end.