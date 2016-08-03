%%%-------------------------------------------------------------------
%%% @author ivan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jun 2016 12:48
%%%-------------------------------------------------------------------
-module(sboli_profiles).
-author("Ivan Romanyuk").

%% API
-include("../stromboli.hrl").
-export([get_profiles_config/2]).


-spec get_profiles_config(ne_binary() | ne_binaries(), ne_binary()) ->
                            wh_json:object().
get_profiles_config(Ids, AccId) when is_list(Ids) ->
    get_profiles_config(Ids, AccId, []);
get_profiles_config(Id, AccId) ->
    get_profiles_config([Id], AccId, []).

-spec get_profiles_config(ne_binaries(), ne_binary(), wh_json:objects() ) ->
                            wh_json:object().
get_profiles_config([Id|Ids], AccId, Profiles) ->
    {'ok', Profile} = open_profile(Id, AccId),
    get_profiles_config(Ids, AccId, [Profile| Profiles]);
get_profiles_config([], _AccId, Profiles) ->
    wh_json:merge_recursive(Profiles).

-spec open_profile(ne_binaries(), ne_binary()) ->
                    {'error', 'not_found'}
                    | {'ok', wh_json:object()}.
open_profile(Id, AccId) ->
    lager:debug([{trace, true}], "Reading profile ~p", [Id]),
    AccDb = wh_util:format_account_id(AccId, 'encoded'),
    case couch_mgr:open_doc(AccDb, Id) of
        {'error', E} ->
            lager:debug([{trace, true}],"Can't read provision profile ~p", [E]),
            {'error', 'not_found'};
        {'ok', JObj} ->
            Policies = wh_json:get_value(<<"policies">>, JObj),
            {'ok', sboli_policies:get_policies_config(Policies, AccId)}
    end.
