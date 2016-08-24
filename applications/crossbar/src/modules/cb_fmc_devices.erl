%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cb_fmc_devices).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,read/1, read/2
         ,put/1
         ,post/2
         ,delete/2
         ,delete_orphaned_fmcs/1
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"fmc_devices/crossbar_listing">>).
-define(BY_ACCOUNT_LIST, <<"fmc_devices/list_by_account_id">>).
-define(BY_FMC_AND_NUMBER_LIST, <<"fmc_devices/list_by_fmc_and_number">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.fmc_devices">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.fmc_devices">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.fmc_devices">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.fmc_devices">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.fmc_devices">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"account.deleted">>, ?MODULE, 'delete_orphaned_fmcs').

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
%% Expected: /fmc_devices
%%           /fmc_devices/${fmc_device_id}
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
%% /fmc_devices might load all FMC devices
%% /fmc_devices/${fmc_device_id} might interoperate with this FMC devices
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_fmc_devices(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_fmc_device(Context, Id, cb_context:req_verb(Context)).

-spec validate_fmc_devices(cb_context:context(), http_method()) -> cb_context:context().
validate_fmc_devices(Context, ?HTTP_GET) ->
    read(Context);
validate_fmc_devices(Context, ?HTTP_PUT) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"fmc_device">>, Context, OnSuccess).

-spec error_fmc_device_not_found(cb_context:context()) -> cb_context:context().
error_fmc_device_not_found(Context) ->
    cb_context:add_validation_error(<<"fmc">>, <<"not_found">>
        ,wh_json:from_list([{<<"message">>, <<"FMC Device with this ID wasn't found">>}])
        ,Context).

-spec error_device_not_found(cb_context:context()) -> cb_context:context().
error_device_not_found(Context) ->
    cb_context:add_validation_error(<<"fmc">>, <<"not_found">>
        ,wh_json:from_list([{<<"message">>, <<"Device with this ID wasn't found">>}])
        ,Context).

-spec error_fmc_device_exist(cb_context:context()) -> cb_context:context().
error_fmc_device_exist(Context) ->
    cb_context:add_validation_error(<<"fmc">>, <<"unique">>
        ,wh_json:from_list([{<<"message">>, <<"FMC Device with this A-Number and FMC Value already exists">>}])
        ,Context).

-spec error_invalid_device_type(cb_context:context()) -> cb_context:context().
error_invalid_device_type(Context) ->
    cb_context:add_validation_error(<<"fmc">>, <<"invalid">>
        ,wh_json:from_list([{<<"message">>, <<"Invalid device type">>}])
        ,Context).

-spec validate_fmc_device(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_fmc_device(Context, Id, ?HTTP_GET) ->
    case couch_mgr:open_cache_doc(?WH_FMC_DB, Id) of
        {'ok', Doc} ->
            AccountId = cb_context:account_id(Context),
            case wh_json:get_value(<<"account_id">>, Doc) of
                AccountId ->
                    Context1 = cb_context:set_doc(Context, Doc),
                    read(Context1, Id);
                _ ->
                    error_fmc_device_not_found(Context)
            end;
        _ ->
            error_fmc_device_not_found(Context)
    end;
validate_fmc_device(Context, Id, ?HTTP_POST) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"fmc_device">>, Context, OnSuccess);
validate_fmc_device(Context, Id, ?HTTP_DELETE) ->
    case couch_mgr:open_cache_doc(?WH_FMC_DB, Id) of
        {'ok', SavedDoc} ->
            AccountId = cb_context:account_id(Context),
            case wh_json:get_value(<<"account_id">>, SavedDoc) of
                AccountId ->
                    Context1 = cb_context:set_doc(Context, SavedDoc),
                    delete(Context1, Id);
                _ ->
                    error_fmc_device_not_found(Context)
            end;
        _ ->
            error_fmc_device_not_found(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec apply_doc_to_context(cb_context:context(), list(wh_json:object())) -> cb_context:context().
apply_doc_to_context(Context, Doc) ->
    Context1 = cb_context:set_doc(Context, Doc),
    Context2 = cb_context:set_resp_data(Context1, Doc),
    cb_context:set_resp_status(Context2, 'success').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context()) -> cb_context:context().
read(Context) ->
    {'ok', JObjs} = couch_mgr:get_results(?WH_FMC_DB, ?BY_ACCOUNT_LIST, [{'key', cb_context:account_id(Context)}]),
    Doc = [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
    apply_doc_to_context(Context, Doc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context(), path_token()) -> cb_context:context().
read(Context, Id) ->
    {'ok', Doc} = couch_mgr:open_cache_doc(?WH_FMC_DB, Id),
    Doc1 = wh_doc:public_fields(Doc),
    apply_doc_to_context(Context, wh_json:delete_keys([<<"a_number">>, <<"account_id">>], Doc1)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Doc = wh_json:set_values([{<<"pvt_type">>, <<"fmc_device">>}
                              ,{<<"account_id">>, cb_context:account_id(Context)}]
                              ,cb_context:doc(Context)),
    DeviceId = wh_json:get_value(<<"device_id">>, Doc),
    {'ok', DeviceDoc} = couch_mgr:open_cache_doc(cb_context:account_db(Context), DeviceId),
    ANumber = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
    NormalizedANumber = wnm_util:normalize_number(ANumber),
    Doc1 = wh_json:set_value(<<"a_number">>, NormalizedANumber, Doc),
    {'ok', ResultDoc} = couch_mgr:save_doc(?WH_FMC_DB, Doc1),
    ResultDoc1 = wh_doc:public_fields(ResultDoc),
    Context1 = cb_context:set_doc(Context, ResultDoc1),
    cb_context:set_resp_data(Context1, ResultDoc1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, Id) ->
    AccountId = cb_context:account_id(Context),
    Doc1 = wh_json:set_values([{<<"_id">>, Id}
                               ,{<<"pvt_type">>, <<"fmc_device">>}
                               ,{<<"account_id">>, AccountId}]
                              ,cb_context:doc(Context)),
    DeviceId = wh_json:get_value(<<"device_id">>, Doc1),
    {'ok', DeviceDoc} = couch_mgr:open_cache_doc(cb_context:account_db(Context), DeviceId),
    ANumber = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
    NormalizedANumber = wnm_util:normalize_number(ANumber),
    Doc2 = wh_json:set_value(<<"a_number">>, NormalizedANumber, Doc1),
    {'ok', ResultDoc} = couch_mgr:save_doc(?WH_FMC_DB, Doc2),
    ResultDoc1 = wh_doc:public_fields(ResultDoc),
    Context1 = cb_context:set_doc(Context, ResultDoc1),
    cb_context:set_resp_data(Context1, ResultDoc1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Id) ->
    {'ok', _} = couch_mgr:del_doc(?WH_FMC_DB, Id),
    cb_context:set_resp_status(Context, 'success').

-spec delete_orphaned_fmcs(cb_context:context()) -> cb_context:context().
delete_orphaned_fmcs(Context) ->
    ViewOptions = [{'key', cb_context:account_id(Context)}],
    case couch_mgr:get_results(?WH_FMC_DB, <<"fmc_devices/by_account_id">>, ViewOptions) of
        {'ok', JObjs} -> couch_mgr:del_docs(?WH_FMC_DB, lists:map(fun get_doc_id/1, JObjs));
        _Err -> lager:error("Can not cleanup fmcs for deleted for ~s due to ~p", [cb_context:account_db(Context), _Err])
    end.

-spec get_doc_id(wh_json:object()) -> ne_binary().
get_doc_id(JObj) ->
    wh_json:get_value(<<"value">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Operations on succesful data validation
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation('undefined' | api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    FMCValue = wh_json:get_value(<<"x_fmc_value">>, cb_context:doc(Context)),
    DeviceId = wh_json:get_value(<<"device_id">>, cb_context:doc(Context)),
    case couch_mgr:open_cache_doc(cb_context:account_db(Context), DeviceId) of
        {'ok', DeviceDoc} ->
            Doc = wh_json:set_value(<<"pvt_type">>, <<"fmc_device">>, cb_context:doc(Context)),
            DeviceType = wh_json:get_value(<<"device_type">>, DeviceDoc),
            Number = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
            case DeviceType of
                <<"cellphone">> ->
                    case is_existing_fmc(Number, FMCValue, 'undefined') of
                        'true' ->
                            error_fmc_device_exist(Context);
                        'false' ->
                            cb_context:set_doc(Context, Doc)
                    end;
                _ ->
                    error_invalid_device_type(Context)
            end;
        {'error', _} ->
            error_device_not_found(Context)
    end;
on_successful_validation(Id, Context) ->
    FMCValue = wh_json:get_value(<<"x_fmc_value">>, cb_context:doc(Context)),
    case couch_mgr:open_cache_doc(?WH_FMC_DB, Id) of
        {'ok', Doc} ->
            case wh_doc:is_soft_deleted(Doc) of
                'true' ->
                    error_device_not_found(Context);
                'false' ->
                    AccountId = cb_context:account_id(Context),
                    case wh_json:get_value(<<"account_id">>, Doc) of
                        AccountId ->
                            DeviceId = wh_json:get_value(<<"device_id">>, Doc),
                            {'ok', DeviceDoc} = couch_mgr:open_cache_doc(cb_context:account_db(Context), DeviceId),
                            case wh_json:get_value(<<"device_type">>, DeviceDoc) of
                                <<"cellphone">> ->
                                    Number = wh_json:get_value([<<"call_forward">>, <<"number">>], DeviceDoc),
                                    case is_existing_fmc(Number, FMCValue, Id) of
                                        'true' ->
                                            error_fmc_device_exist(Context);
                                        'false' ->
                                            PrivJObj = wh_json:private_fields(Doc),
                                            NewDoc = wh_json:merge_jobjs(PrivJObj, cb_context:doc(Context)),
                                            cb_context:set_doc(Context, NewDoc)
                                    end;
                                _ ->
                                    error_invalid_device_type(Context)
                            end;
                        _ ->
                            error_fmc_device_not_found(Context)
                    end
            end;
        _ ->
            error_fmc_device_not_found(Context)
    end.

check_fmc_docid(_JObj, 'undefined') ->
    'true';
check_fmc_docid(JObj, FMCDocId) when is_binary(FMCDocId) ->
    wh_doc:id(JObj) =/= FMCDocId.

-spec is_existing_fmc(ne_binary(), ne_binary(), binary() | 'undefined') -> boolean().
is_existing_fmc(Number, FMCValue, FMCDocId) ->
    NormalizedANumber = wnm_util:normalize_number(Number),
    case couch_mgr:get_results(?WH_FMC_DB
                               ,?BY_FMC_AND_NUMBER_LIST
                               ,[{'startkey', [NormalizedANumber, FMCValue]}
                                 ,{'endkey', [NormalizedANumber, FMCValue]}]) of
        {'ok', JObjs} ->
            Doc = [{wh_json:get_value([<<"value">>, <<"a_number">>], JObj)
                    ,wh_json:get_value([<<"value">>, <<"x_fmc_value">>], JObj)
                   } || JObj <- JObjs, check_fmc_docid(JObj, FMCDocId)],
            length(Doc) > 0;
        {'error', Error} ->
            lager:error("error on find operation: ~p", [Error]),
            'false'
    end.
