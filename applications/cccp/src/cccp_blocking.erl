%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cccp_blocking).

-behaviour(gen_server).

-include("cccp.hrl").

-record(state, {}).
-type state() :: #state{}.

-define(BLOCK_TIMEOUT, whapps_config:get(?CCCP_CONFIG_CAT, <<"blocking_period">>, ?SECONDS_IN_HOUR)).
-define(MAX_RETRIES, whapps_config:get(?CCCP_CONFIG_CAT, <<"maximum_retries">>, 5)).
-define(CRAWLER_INTERVAL, whapps_config:get(?CCCP_CONFIG_CAT, <<"blocking_clear_interval">>, ?MILLISECONDS_IN_HOUR)).

%% API
-export([is_cid_blocked/1
         ,block_cid/1
         ,clear_blocking/1
        ]).

%% gen_server
-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-spec is_cid_blocked(ne_binary()) -> boolean().
is_cid_blocked(CID) ->
    case couch_mgr:open_cache_doc(?KZ_CCCPS_DB, CID) of
        {'ok', JObj} -> wh_json:get_integer_value(<<"till">>, JObj) > wh_util:current_tstamp();
        _ -> 'false'
    end.

-spec block_cid(ne_binary()) -> 'ok'.
block_cid(CID) ->
    couch_mgr:update_cache_doc(?KZ_CCCPS_DB, CID, fun perform_cid_blocking/1, perform_cid_blocking(wh_json:new())).

-spec perform_cid_blocking(wh_json:object()) -> wh_json:object().
perform_cid_blocking(JObj) ->
    Count = wh_json:get_integer_value(<<"count">>, JObj, 0) + 1,
    Till = case Count >= ?MAX_RETRIES of
               'true' -> <<"forever">>;
               'false' -> wh_util:current_tstamp() + ?BLOCK_TIMEOUT
           end,
    JObj1 = wh_json:set_values([{<<"till">>, Till}
                                ,{<<"count">>, Count}
                               ], JObj),
    wh_doc:update_pvt_parameters(JObj1, ?KZ_CCCPS_DB, [{'type', <<"cid_blocking">>}]).

-spec clear_blocking(ne_binary()) -> 'ok'.
clear_blocking(CID) ->
    couch_mgr:del_doc(?KZ_CCCPS_DB, CID).

-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec init(list()) -> {'ok', state()}.
init([]) ->
    send_clear(),
    {'ok', #state{}}.

handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

handle_cast(_Msg, State) ->
    lager:info("Unhandled msg: ~p", [_Msg]),
    {'noreply', State}.

handle_info('clear', State) ->
    ViewOptions = [{'descending', 'true'}
                   ,{'startkey', wh_util:current_tstamp()}
                  ],
    case couch_mgr:get_results(?KZ_CCCPS_DB, <<"blocking/expired">>, ViewOptions) of
        {'ok', JObjs} -> couch_mgr:del_docs(?KZ_CCCPS_DB, [wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs]);
        _ -> 'ok'
    end,
    send_clear(),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec send_clear() -> 'ok'.
send_clear() ->
    timer:send_after(?CRAWLER_INTERVAL, self(), 'clear'),
    'ok'.
