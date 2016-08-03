%%%-------------------------------------------------------------------
%%% @author ivan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jun 2016 12:13
%%%-------------------------------------------------------------------
-module(sboli_policies).
-author("Ivan Romanyuk").

%% API
-export([get_policies_config/2
        ]).

-include("../stromboli.hrl").
-define(POLICIES_DB, <<"provision_policies">>).


-spec get_policies_config(ne_binaries() | ne_binary() , ne_binary()) ->
                            wh_json:object().
get_policies_config(Ids, AccId) when is_list(Ids) ->
    get_policies_config(Ids, AccId, []);
get_policies_config(Id, AccId) ->
    get_policies_config([Id], AccId, []).

-spec get_policies_config(ne_binaries(), ne_binary(), wh_json:objects() ) ->
                            wh_json:object().
get_policies_config([Id|Ids], AccId, Policies) ->
    {'ok', Policy} = open_policy(Id, AccId),
    get_policies_config(Ids, AccId, [Policy| Policies]);
get_policies_config([], _AccId, Policies) ->
    wh_json:merge_recursive(Policies).

-spec open_policy(ne_binaries(), ne_binary()) ->
                    {'error', 'not_found'}
                    | {'ok', wh_json:object()}.
open_policy(Id, AccId) ->
    lager:debug([{trace, true}], "Reading policy ~p", [Id]),
    AccDb = wh_util:format_account_id(AccId, 'encoded'),
    case couch_mgr:open_doc(AccDb, Id) of
        {'error', E} ->
            lager:debug([{trace, true}],"Can't read provision policy ~p", [E]),
            {'error', 'not_found'};
        {'ok', JObj} ->
            Config = wh_json:get_value(<<"config">>, JObj),
            {'ok', Config}
    end.
