%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Ivan Romanyuk
%%%
%%%-------------------------------------------------------------------
-module(cb_unit_options).

-export([init/0
    ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3, allowed_methods/4
    ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3, resource_exists/4
    ,validate/1, validate/2, validate/3, validate/4, validate/5
    ,save/1, save/2, save/3, save/4
    ,delete/2, delete/3, delete/4
    ,authorize/1
]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"opts/crossbar_listing">>).
-define(LIST_CHILDREN, <<"opts/listing_by_children">>).
-define(LISTING_BY_PATH, <<"opts/listing_by_path">>).
-define(PHONE_CONFIGS, <<"phone_configs">>).
-define(URL_CONFIG, <<"_config">>).
-define(URL_HICONFIG, <<"_hiconfig">>).
-define(JSON_CONFIG, <<"!_config">>).
-define(PROPERTIES, <<"properties">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.unit_options">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.unit_options">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.unit_options">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.unit_options">>, ?MODULE, 'save'),
    _ = crossbar_bindings:bind(<<"*.execute.post.unit_options">>, ?MODULE, 'save'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.unit_options">>, ?MODULE, 'delete').


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
  [?HTTP_GET].
allowed_methods(_) ->
  [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_, _) ->
  [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_, _, _) ->
  [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].
allowed_methods(_, _, _, _) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].



%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, _) -> 'true'.
resource_exists(_, _, _) -> 'true'.
resource_exists(_, _, _, _) -> 'true'.


%%--------------------------------------------------------------------
authorize(_) ->
    'true'.
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /skels mights load a list of skel objects
%% /skels/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()
               ,path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()
               ,path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_opts(Context, cb_context:req_verb(Context)).
validate(Context, P1) ->
    Path = list_to_lower([P1]),
    validate_unit_options(Context, Path, cb_context:req_verb(Context)).
validate(Context, P1, P2) ->
    Path = list_to_lower([P2, P1]),
    validate_unit_options(Context, Path, cb_context:req_verb(Context)).
validate(Context, P1, P2, P3) ->
    Path = list_to_lower([P3, P2, P1]),
    validate_unit_options(Context, Path, cb_context:req_verb(Context)).
validate(Context, P1, P2, P3, P4) ->
    Path = list_to_lower([P4, P3, P2, P1]),
    validate_unit_options(Context, Path, cb_context:req_verb(Context)).

-spec validate_opts(cb_context:context(), http_method()) -> cb_context:context().
validate_opts(Context, ?HTTP_GET) ->
    list_children([], Context).

-spec validate_unit_options(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_unit_options(Context, [?URL_CONFIG|Path], ?HTTP_GET) ->
    read_config(lists:reverse(Path), Context);
validate_unit_options(Context, [?URL_HICONFIG|Path], ?HTTP_GET) ->
    read_hierarchical_config(lists:reverse(Path), Context);
validate_unit_options(Context, Path, ?HTTP_GET) ->
    list_children(lists:reverse(Path), Context);
validate_unit_options(Context, Path, ?HTTP_POST) ->
    update(lists:reverse(Path), Context);
validate_unit_options(Context, Path, ?HTTP_DELETE) ->
    validate_delete(lists:reverse(Path), Context);
validate_unit_options(Context, Path, ?HTTP_PUT) ->
    create(lists:reverse(Path), Context).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec save(cb_context:context()) -> cb_context:context().
save(Context) ->
  case couch_mgr:save_doc(?PHONE_CONFIGS, cb_context:doc(Context)) of
      {'ok', JObj} ->
          lager:debug([{trace, true}], "!!!!!!!!!! Document saved ~p", [JObj]),
          crossbar_doc:handle_couch_mgr_success(JObj, Context);
      {'error', Error} ->
          crossbar_doc:handle_couch_mgr_errors(Error, 'undefined', Context)
  end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec save(cb_context:context(), path_token()) -> cb_context:context().
-spec save(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec save(cb_context:context(), path_token(), path_token()
           ,path_token()) -> cb_context:context().
save(Context, _) ->
    save(Context).

save(Context, _, _) ->
    save(Context).

save(Context, _, _, _) ->
    save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()
             ,path_token()) -> cb_context:context().
delete(Context, _Vendor) ->
    delete_root(Context).
delete(Context, Vendor, Family) ->
    delete_child([Vendor, Family], Context).
delete(Context, Vendor, Family, Model) ->
    delete_child([Vendor, Family, Model], Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_delete(path_token(), cb_context:context()) -> cb_context:context().
validate_delete(Path, Context) ->
    case get_id_by_path(Path, Context) of
        {'ok', 'undefined', _} ->  crossbar_util:response('error'
                                                          ,<<"bad identifier">>
                                                          ,404
                                                          ,Path
                                                          ,Context);
        {'ok', Id, _} -> Doc = wh_json:set_value(<<"id">>, Id, wh_json:new()),
            Context1 = cb_context:set_doc(Context, Doc),
            cb_context:set_resp_status(Context1, 'success');
        {'error', _, Context1} -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_root(cb_context:context()) -> cb_context:context().
delete_root(Context) ->
    Id = wh_json:get_value(<<"id">>, cb_context:doc(Context)),
    case couch_mgr:open_doc(?PHONE_CONFIGS, Id) of
        {'ok', JObj} ->
            JObj1 = wh_json:get_value(?PROPERTIES, JObj),
            do_delete_root(Id, cb_context:set_resp_data(Context, JObj1));
        {'error', Error} ->
            crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec do_delete_root(ne_binary(), cb_context:context()) -> cb_context:context().
do_delete_root(Id, Context) ->
    case couch_mgr:del_doc(?PHONE_CONFIGS, Id) of
        {'ok', _} ->
            cb_context:set_resp_status(Context, 'success');
        {'error', Error} ->
            crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_child(path_token(), cb_context:context()) -> cb_context:context().
delete_child(Path, Context) ->
    Id = wh_json:get_value(<<"id">>, cb_context:doc(Context)),
    case couch_mgr:open_doc(?PHONE_CONFIGS, Id) of
        {'ok', JObj} ->
            lager:debug([{trace, true}], "!!!!!!!!!! Doc to remove ~p", [JObj]),
            Context1 = cb_context:set_doc(Context, JObj),
            do_delete_child(Path, Context1);
        {'error', Error} ->
            crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec do_delete_child(path_token(), cb_context:context()) -> cb_context:context().
do_delete_child(Path, Context)->
    Path1 = [?PROPERTIES|Path],
    JObj = cb_context:doc(Context),
    JObjToDel = wh_json:get_value(Path1, JObj),
    JObj1 = wh_json:delete_key(Path1, JObj),
    lager:debug([{trace, true}], "!!!!!!!!!! Doc to replace ~p", [JObj1]),
    case couch_mgr:save_doc(?PHONE_CONFIGS, JObj1) of
        {'ok', _} ->
            crossbar_doc:handle_couch_mgr_success(JObjToDel, Context);
        {'error', Error} ->
            Id = wh_json:get_value(<<"id">>, JObj),
            crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), cb_context:context()) -> cb_context:context().
create(Path, Context) ->
    case get_id_by_path(Path, Context) of
        {'ok', 'undefined', _} when length(Path) =:= 1 -> create_root(Path, Context);
        {'ok', 'undefined', _} -> create_child(Path, Context);
        {'ok', _Id, _} -> crossbar_util:response_conflicting_docs(Context);
        {'error', _, Context1} -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_root(ne_binary(), cb_context:context()) -> cb_context:context().
create_root(Path, Context) ->
    lager:debug([{trace, true}], "!!!!!!!!!! Context req_data ~p", [cb_context:req_data(Context)]),
    OnSuccess = fun(C) -> on_successful_put_validation(Path, 'undefined', C) end,
    cb_context:validate_request_data(<<"opts">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_child(ne_binary(), cb_context:context()) -> cb_context:context().
create_child(Path, Context) ->
    ParentPath = lists:reverse(tl(lists:reverse(Path))),
    case get_id_by_path(ParentPath, Context) of
        {'ok', 'undefined', _} -> crossbar_util:response('error'
                                                         ,<<"bad parent path">>
                                                         ,404
                                                         ,ParentPath
                                                         ,Context);
        {'ok', Id, _} ->
            OnSuccess = fun(C) -> on_successful_put_validation(Path, Id, C) end,
            cb_context:validate_request_data(<<"opts">>, Context, OnSuccess);
        {'error', _, Context1} -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec list_to_lower(binaries()) -> binaries().
list_to_lower(BinStrings) when is_list(BinStrings)->
    [to_lower(E) || E <- BinStrings].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec to_lower(binary()) -> binary().
to_lower(Value) when is_binary(Value)->
    list_to_binary(string:to_lower(binary_to_list(Value))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Path, Context) ->
    case get_id_by_path(Path, Context) of
        {'ok', 'undefined', _} ->  crossbar_util:response('error'
                                                          ,<<"bad identifier">>
                                                          ,404
                                                          ,Path
                                                          ,Context);
        {'ok', Id, _} ->
            OnSuccess = fun(C) -> on_successful_post_validation(Path, Id, C) end,
            cb_context:validate_request_data(<<"opts">>, Context, OnSuccess);
        {'error', _, Context1} -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec list_children(path_tokens(), cb_context:context()) -> cb_context:context().
list_children(Path, Context) ->
    lager:debug([{trace, true}], "Path is ~p", [Path]),
    crossbar_doc:load_view(?LIST_CHILDREN
                           ,[{'databases', [?PHONE_CONFIGS]}
                             ,{'key', Path}
                            ]
                           ,Context
                           ,fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_put_validation(path_tokens(), ne_binary()
                                   ,cb_context:context()) -> cb_context:context().
on_successful_put_validation([Vendor], 'undefined', Context) ->
    lager:debug([{trace, true}], "!!!!!!!!!!!!!!!!!!!! Context doc ~p", [cb_context:doc(Context)]),
    Doc1 = wh_json:set_value([?PROPERTIES, Vendor], cb_context:doc(Context), wh_json:new()),
    lager:debug([{trace, true}], "!!!!!!!!!!!!!!!!!!!! NewDoc doc ~p", [Doc1]),
    cb_context:set_doc(Context, Doc1);
on_successful_put_validation(Path, Id, Context) ->
    Path1 = [?PROPERTIES|Path],
    Doc1 = wh_json:set_value(Path1, cb_context:doc(Context), wh_json:new()),
    load_merge(Id, Doc1, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_post_validation(path_tokens(), ne_binary()
                                    ,cb_context:context()) -> cb_context:context().
on_successful_post_validation(Path, Id, Context) ->
    Path1 = [?PROPERTIES|Path] ++ [?JSON_CONFIG],
    load_update(Id, Path1, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec read_config(path_tokens(), cb_context:context()) -> cb_context:context().
-spec read_config(ne_binary(), path_tokens()
                  ,cb_context:context()) -> cb_context:context().
read_config(Path, Context) ->
    {Id, Context1} = get_id_by_path_or_response(Path, Context),
    case cb_context:resp_status(Context1) of
        'success' -> read_config(Id, Path, Context1);
        _Status -> Context1
    end.

read_config(Id, Path, Context) ->
    case couch_mgr:open_doc(?PHONE_CONFIGS, Id) of
        {'ok', JOBj} -> get_config(Path, JOBj, Context);
        {'error', Error} -> crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec read_hierarchical_config(path_tokens(), cb_context:context()) -> cb_context:context().
-spec read_hierarchical_config(ne_binary(), path_tokens()
                               ,cb_context:context()) -> cb_context:context().
read_hierarchical_config(Path, Context) ->
    {Id, Context1} = get_id_by_path_or_response(Path, Context),
    case cb_context:resp_status(Context1) of
        'success' -> read_hierarchical_config(Id, Path, Context1);
        _Status -> Context1
    end.

read_hierarchical_config(Id, Path, Context) ->
    case couch_mgr:open_doc(?PHONE_CONFIGS, Id) of
        {'ok', JOBj} -> get_hierarchical_config(Path, [?PROPERTIES], JOBj, [], Context);
        {'error', Error} -> crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_config(path_tokens(), wh_json:object(), cb_context:context()) -> cb_context:context().
get_config(Path, JOBj, Context) ->
    Path1 = [?PROPERTIES|Path] ++ [?JSON_CONFIG],
    case wh_json:get_value(Path1, JOBj) of
        'undefined' -> lager:debug([{trace, true}], "Undefined _config value at path ~p", [Path1]),
            crossbar_util:response('error', <<"Bad record in database">>);
        JObj -> Config = wh_json:set_value(<<"_config">>, JObj, wh_json:new()),
            cb_context:set_resp_data(Context, Config)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_hierarchical_config(path_tokens(), binaries(), wh_json:object()
                              ,wh_json:objecs(), cb_context:context()) -> cb_context:context().
get_hierarchical_config([P|PathRest], CurrentPath, JObj, Configs, Context) ->
    Path1 = CurrentPath ++ [P],
    case wh_json:get_value(Path1 ++ [?JSON_CONFIG], JObj) of
        'undefined' -> lager:debug([{trace, true}], "Undefined _config value at path ~p", [Path1]),
            crossbar_util:response('error', <<"Bad record in database">>);
        Config -> get_hierarchical_config(PathRest, Path1, JObj, [Config|Configs], Context)
    end;
get_hierarchical_config([], _CurrentPath, _JObj, Configs, Context) ->
    Config = wh_json:merge_recursive(lists:reverse(Configs)),
    Config1 = wh_json:set_value(<<"_config">>, Config, wh_json:new()),
    cb_context:set_resp_data(Context, Config1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_id_by_path_or_response(path_tokens(),  cb_context:context()) ->
                                    {'undefined', cb_context:context()}
                                    | {'ok', binary(), cb_context:context()}.
get_id_by_path_or_response(Path, Context) ->
    case get_id_by_path(Path, Context) of
        {'ok', 'undefined', Context1} ->
            {'undefined'
             ,crossbar_util:response('error'
                                     ,<<"bad identifier">>
                                     ,404
                                     ,Path
                                     ,Context1)
             };
        {'ok', Id, Context1}  -> {Id, Context1};
        {'error', _, Context1} -> {'undefined', Context1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_id_by_path(path_tokens(), cb_context:context()) ->
                        {'ok', 'undefined' | binary(), cb_context:context()}
                        | {'error', 'undefined', cb_context:context()}.
get_id_by_path(Path, Context) ->
    Options = [{'databases', [?PHONE_CONFIGS]}
               ,{'key', Path}
              ],
    Context1 = crossbar_doc:load_view(?LISTING_BY_PATH, Options, Context),
    case cb_context:resp_status(Context1) of
        'success' -> case cb_context:doc(Context1) of
                         [] ->{'ok','undefined', Context1};
                         [JObj] ->
                             {'ok', wh_json:get_value(<<"value">>, JObj), Context1}
                     end;
        _Status -> {'error', 'udefined', Context1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load_merge(ne_binary(), wh_json:object(), cb_context:context()) -> cb_context:context().
load_merge(Id, Doc, Context) ->
    case couch_mgr:open_doc(?PHONE_CONFIGS, Id) of
        {'ok', JOBj} -> JOBj1 = wh_json:merge_recursive(Doc, JOBj),
            crossbar_doc:handle_couch_mgr_success(JOBj1, Context);
        {'error', Error} ->
            crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load_update(ne_binary(), path_tokens(), cb_context:context()) -> cb_context:context().
load_update(Id, Path, Context) ->
    case couch_mgr:open_doc(?PHONE_CONFIGS, Id) of
        {'ok', JOBj} -> Config = wh_json:get_value(?JSON_CONFIG, cb_context:doc(Context)),
            JOBj1 = wh_json:set_value(Path, Config, JOBj),
            crossbar_doc:handle_couch_mgr_success(JOBj1, Context);
        {'error', Error} ->
            crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.