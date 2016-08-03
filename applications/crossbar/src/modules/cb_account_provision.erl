%%%-------------------------------------------------------------------
%%% @author ivan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2016 13:14
%%%-------------------------------------------------------------------
-module(cb_account_provision).
-author("Ivan Romanyuk").

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,post/2
        ,delete/2
        ]).

-include("../crossbar.hrl").

-define(LISTING_PROVISION_CONFIG, <<"stromboli/account_provision_listing">>).
-define(HIERARCHICAL, <<"_hierarchical">>).


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
  _ = crossbar_bindings:bind(<<"*.allowed_methods.account_provision">>, ?MODULE, 'allowed_methods'),
  _ = crossbar_bindings:bind(<<"*.resource_exists.account_provision">>, ?MODULE, 'resource_exists'),
  _ = crossbar_bindings:bind(<<"*.validate.account_provision">>, ?MODULE, 'validate'),
  _ = crossbar_bindings:bind(<<"*.execute.get.account_provision">>, ?MODULE, 'get'),
  _ = crossbar_bindings:bind(<<"*.execute.put.account_provision">>, ?MODULE, 'put'),
  _ = crossbar_bindings:bind(<<"*.execute.post.account_provision">>, ?MODULE, 'post'),
  _ = crossbar_bindings:bind(<<"*.execute.delete.account_provision">>, ?MODULE, 'delete').


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
  [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
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
resource_exists() -> true.
resource_exists(_) -> true.

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
validate(Context) ->
  validate_account_provision(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
  validate_account_provision(Context, Id, cb_context:req_verb(Context)).

-spec validate_account_provision(cb_context:context(), http_method()) -> cb_context:context().
validate_account_provision(Context, ?HTTP_GET) ->
  summary(Context);
validate_account_provision(Context, ?HTTP_PUT) ->
  maybe_create(Context).

-spec validate_account_provision(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_account_provision(Context, ?HIERARCHICAL, ?HTTP_GET) ->
    get_hierarchical_config(Context);
validate_account_provision(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_account_provision(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_account_provision(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_hierarchical_config(cb_context:context()) -> cb_context:context().
get_hierarchical_config(Context) ->
    Id = cb_context:account_id(Context),
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, Id) of
        {'ok', JObj} ->
            [_UnexistingRoot | Tree] = kz_account:tree(JObj),
            Ids = Tree ++ [Id],
            {Config, Locks} = do_get_hierarchical_config(Ids),
            Result1 = wh_json:set_value(<<"config">>, Config, wh_json:new()),
            Result2 = wh_json:set_value(<<"locks">>, Locks, Result1),
            crossbar_doc:handle_couch_mgr_success(Result2, Context);
        {'error', Error} ->
            crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec do_get_hierarchical_config(ne_binaries()) -> cb_context:context().
do_get_hierarchical_config(Tree) ->
    Configs1 = lists:foldr(fun(Id, Configs) ->
                                case get_account_config(wh_util:format_account_db(Id)) of
                                    {'ok', 'undefined'} -> Configs;
                                    {'ok', JObj} ->
                                        Config = wh_json:get_value(<<"config">>, JObj),
                                        Locks = wh_json:get_value(<<"locks">>, JObj),
                                        [{Config, Locks} | Configs]
                                end
                           end, [], Tree),
    lager:debug([{trace, true}], "Configs:  ~p", [Configs1]),
    mc_json:merge_recursive(Configs1).

%%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec maybe_create(cb_context:context()) -> cb_context:context().
maybe_create(Context) ->
    AccDb = cb_context:account_db(Context),
case get_account_config(AccDb) of
    {'ok', 'undefined'} -> create(Context);
    {'ok', Id} ->
        lager:debug([{trace, true}], "Config document already exists:  ~p", [Id]),
        crossbar_util:response('error', <<"Config document already exists">>, Context);
    {'error', Msg} ->
        crossbar_util:response('error', Msg, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
  lager:debug([{trace, true}], "create ~p", [Context]),
  OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
  cb_context:validate_request_data(<<"devicemanager_accounts">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    AccDb = cb_context:account_db(Context),
    case couch_mgr:open_doc(AccDb, Id) of
        {'ok', JObj} ->
            crossbar_doc:handle_couch_mgr_success(JObj, Context);
        {'error', Error} ->
            crossbar_doc:handle_couch_mgr_errors(Error, Id, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
  OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
  cb_context:validate_request_data(<<"devicemanager_accounts">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?LISTING_PROVISION_CONFIG, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
  cb_context:set_doc(Context, wh_doc:set_type(cb_context:doc(Context), <<"provision_config">>));
on_successful_validation(Id, Context) ->
  crossbar_doc:load_merge(Id, Context).

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
-spec get_account_config(ne_binary()) ->
                            {'ok',wh_json:object() }
                            | {'error', 'too_many_config_docs'}
                            | couch_mgr:couchbeam_error().
get_account_config(AccDb) ->
    lager:debug([{trace, true}], "!!!!!!!!!!!!! AccDb ~p ", [AccDb]),
    case couch_mgr:get_results(AccDb, ?LISTING_PROVISION_CONFIG) of
        {'ok', [JObj]} ->
            Id = wh_json:get_value(<<"value">>, JObj),
            case couch_mgr:open_doc(AccDb, Id) of
                {ok, _JObj1} = R -> R;
                {'error', _} = E1 -> E1
            end;
        {'ok', [_|_]} ->
            {'error', 'too_many_config_docs'};
        {'ok', []} ->
            {'ok', 'undefined'};
        {'error', _} = E -> E
    end.
