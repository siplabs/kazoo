%%%-------------------------------------------------------------------
%%% @copyright 
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(cb_cccps).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
         ,delete_orphaned_cccps/1
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"cccps/crossbar_listing">>).

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
    maybe_init_db(),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.cccps">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.cccps">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.cccps">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.cccps">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.cccps">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.cccps">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"account.deleted">>, ?MODULE, 'delete_orphaned_cccps').

-spec maybe_init_db() -> 'ok'.
maybe_init_db() ->
    case couch_mgr:db_exists(?KZ_CCCPS_DB) of
        'true' -> 
             _ = couch_mgr:revise_doc_from_file(?KZ_CCCPS_DB, 'crossbar', <<"views/cccps.json">>),
            'ok';
        'false' -> init_db()
    end.

-spec init_db() -> 'ok'.
init_db() ->
    couch_mgr:db_create(?KZ_CCCPS_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_CCCPS_DB, 'crossbar', <<"views/cccps.json">>),
    'ok'.

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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /cccps => []
%%    /cccps/foo => [<<"foo">>]
%%    /cccps/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /cccps mights load a list of cccp objects
%% /cccps/123 might load the cccp object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_cccps(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_cccp(Context, Id, cb_context:req_verb(Context)).

-spec validate_cccps(cb_context:context(), http_method()) -> cb_context:context().
validate_cccps(Context, ?HTTP_GET) ->
    summary(Context);
validate_cccps(Context, ?HTTP_PUT) ->
    case get_auth_pin(Context) of
        {'ok', AuthPin} -> check_pin(AuthPin, Context);
        {'error', Msg} ->
            cb_context:add_validation_error(
                <<"cccp">>
                ,<<"unique">>
                ,wh_json:from_list([
                    {<<"message">>, Msg}
                 ])
                ,Context
            )
    end.

-spec validate_cccp(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_cccp(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_cccp(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_cccp(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

-spec get_auth_pin(cb_context:context()) -> {'ok', ne_binary()} | {'error', ne_binary()}.
get_auth_pin(Context) ->
    CID = wh_json:get_value(<<"cid">>, cb_context:req_data(Context), <<"*">>),
    NormalizedCID = wnm_util:normalize_number(CID),
    PIN = wh_json:get_value(<<"pin">>, cb_context:req_data(Context), <<"*">>),
    case [CID, PIN] of
        [<<"*">>, <<"*">>] -> {'error', <<"Wrong combo of CID & PIN">>};
        _ -> {'ok', [NormalizedCID, PIN]}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context2 = crossbar_doc:save(Context),
    couch_mgr:ensure_saved(?KZ_CCCPS_DB, cb_context:doc(Context2)),
    Context2.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    Context2 = crossbar_doc:save(Context),
    couch_mgr:ensure_saved(?KZ_CCCPS_DB, cb_context:doc(Context2)),
    Context2.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    Context2 = crossbar_doc:delete(Context),
    case cb_context:resp_status(Context2) of
        'success' ->
            _ = couch_mgr:del_doc(?KZ_CCCPS_DB, wh_json:get_value(<<"_id">>, cb_context:doc(Context2))),
            Context2;
        _ ->
            Context2
    end.

-spec delete_orphaned_cccps(cb_context:context()) -> cb_context:context().
delete_orphaned_cccps(Context) ->
    ViewOptions = [{'key', cb_context:account_id(Context)}],
    case couch_mgr:get_results(?KZ_CCCPS_DB, <<"cccps/by_account_id">>, ViewOptions) of
        {'ok', JObjs} -> couch_mgr:del_docs(?KZ_CCCPS_DB, lists:map(fun get_doc_id/1, JObjs));
        _Err -> lager:error("Can not cleanup cccps for deleted for ~s due to ~p", [cb_context:account_db(Context), _Err])
    end.

-spec get_doc_id(wh_json:object()) -> ne_binary().
get_doc_id(JObj) ->
    wh_json:get_value(<<"value">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"cccps">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

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
    cb_context:validate_request_data(<<"cccps">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, wh_json:set_value(<<"pvt_type">>, <<"cccp">>, cb_context:doc(Context)));
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
%% Checks whether cid and pin are unique
%% @end
%%--------------------------------------------------------------------

-spec check_pin(ne_binary(), cb_context:context()) -> cb_context:context().
check_pin(AuthPin, Context) ->
    case unique_pin(AuthPin) of
        {'error', 'empty'} -> create(Context);
        _ ->
            cb_context:add_validation_error(
                <<"cccp">>
                ,<<"unique">>
                ,wh_json:from_list([
                    {<<"message">>, <<"Pin already exists">>}
                    ,{<<"cause">>, AuthPin}
                 ])
                ,Context
            )
    end.

-spec unique_pin(ne_binary()) -> cccp_auth:cccp_auth_ret().
unique_pin(AuthPin) ->
    cccp_auth:authorize(AuthPin, <<"cccps/pin_listing">>).
