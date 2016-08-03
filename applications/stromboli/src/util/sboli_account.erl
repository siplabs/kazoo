%%%-------------------------------------------------------------------
%%% @author ivan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2016 17:55
%%%-------------------------------------------------------------------
-module(sboli_account).
-author("Ivan Romanyuk").

%% API
-export([get_account_config/1
        ]).


-include("../stromboli.hrl").
-define(LISTING_PROVISION_CONFIG, <<"stromboli/account_provision_listing">>).

-spec get_account_config(ne_binary()) -> wh_json:object().
get_account_config(Id) ->
    {'ok', JObj} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, Id),
    [_NotExitingRoot|Tree] = lists:append(kz_account:tree(JObj), [Id]),
    lager:debug([{trace, true}],"Tree is ~p", [Tree]),
    Data = [Config || Id1 <- Tree,
                      begin
                          {'ok', Config} = get_provision_config_data(Id1),
                          Config =/= 'undefined'
                      end],
    mc_json:merge_recursive(Data).

-spec get_provision_config_data(ne_binary()) ->
                                    'undefined'
                                    | {wh_json:object(), wh_json:object()}.
get_provision_config_data(Id) ->
    AccDb = wh_util:format_account_db(Id),
    case couch_mgr:get_results(AccDb, ?LISTING_PROVISION_CONFIG) of
        {'ok', [JObj]} ->
            Id1 = wh_json:get_value(<<"value">>, JObj),
            case couch_mgr:open_doc(AccDb, Id1) of
                {ok, JObj1} ->
                    Config = wh_json:get_value(<<"config">>, JObj1),
                    Locks = wh_json:get_value(<<"locks">>, JObj1),
                    {'ok', {Config, Locks}};
                {'error', _} = E1 -> E1
            end;
        {'ok', [_|_]} ->
            {'error', 'too_many_config_docs'};
        {'ok', []} ->
            {'ok', 'undefined'};
        {'error', _} = E -> E
    end.


