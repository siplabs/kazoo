%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz INC
%%% @doc
%%%
%%% (Re-)record media
%%%
%%% "data":{
%%%   "id":{{media_id}},
%%%   "name":{{media_name}}
%%% }
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_record_media).

-include("../callflow.hrl").

%% API
-export([handle/2]).

-define(SOURCE_TYPE, <<"cf_record_media">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    ExistingMediaId = wh_json:get_binary_value(<<"id">>, Data),
    MediaDoc = case couch_mgr:open_cache_doc(whapps_call:account_db(Call), ExistingMediaId) of
                   {'ok', JObj} -> JObj;
                   _Error ->
                       lager:debug("can not open ~s due to ~p, create new", [ExistingMediaId, _Error]),
                       AutoName = iolist_to_binary(["record_media-", wh_util:current_tstamp()]),
                       DesiredMediaName = wh_json:get_binary_value(<<"name">>, Data, AutoName),
                       new_media_doc(DesiredMediaName, Call)
               end,
    lager:info("MediaDoc: ~p", [MediaDoc]),
    MediaName = wh_json:get_value(<<"name">>, MediaDoc),
    case whapps_call_command:b_record(MediaName, ?ANY_DIGIT, Call) of
        {'ok', _} -> save_media(MediaName, MediaDoc, Call);
        'ok' -> save_media(MediaName, MediaDoc, Call);
        _Else -> leger:error("can not record media due to ~p", [_Else])
    end,
    cf_exe:continue(Call).

-spec save_media(ne_binary(), wh_json:object(), whapps_call:call()) -> 'ok'.
save_media(MediaName, MediaDoc, Call) ->
    case wh_doc:maybe_remove_attachments(MediaDoc) of
        {'true', Removed} ->
            {'ok', _} = couch_mgr:save_doc(whapps_call:account_db(Call), Removed),
            lager:info("removed attachment from ~s", [wh_doc:id(MediaDoc)]);
        _ -> 'ok'
    end,
    {'ok', AttachmentUrl} = wh_media_url:store(whapps_call:account_db(Call), wh_doc:id(MediaDoc), MediaName),
    lager:info("store to ~s", [AttachmentUrl]),
    whapps_call_command:b_store(MediaName, AttachmentUrl, Call),
    whapps_call_command:flush_media_cache(wh_media_util:media_path(wh_doc:id(MediaDoc), whapps_call:account_id(Call)), Call).

-spec new_media_doc(ne_binary(), whapps_call:call()) -> ne_binary().
new_media_doc(Name, Call) ->
    AccountDb = whapps_call:account_db(Call),
    Props = props:filter_undefined(
              [{<<"name">>, Name}
               ,{<<"description">>, <<"cf_record_media recorded/prompt media">>}
               ,{<<"source_type">>, ?SOURCE_TYPE}
               ,{<<"source_id">>, <<"none">>}
               ,{<<"owner_id">>, whapps_call:owner_id(Call)}
               ,{<<"language">>, whapps_call:language(Call)}
               ,{<<"media_source">>, <<"recording">>}
               ,{<<"streamable">>, 'true'}
              ]),
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), AccountDb, [{'type', <<"media">>}]),
    {'ok', JObj} = couch_mgr:save_doc(AccountDb, Doc),
    JObj.
