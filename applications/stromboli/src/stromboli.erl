%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stromboli).

-include("stromboli.hrl").

-export([start_link/0
         ,start/0
         ,stop/0
         ,priv_dir/0
         ,reply/3
         ,reply/4
         ,path/1
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    put('callid', ?LOG_SYSTEM_ID),
    _ = start_deps(),
    _ = declare_exchanges(),
    maybe_start_http(),
    maybe_start_https(),
    maybe_start_tftp(),
    stromboli_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the application
%% @end
%%--------------------------------------------------------------------
-spec start() -> 'ok' | {'error', _}.
start() ->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() -> 'ok'.

-spec priv_dir() -> file:filename() | {error, bad_name}.
priv_dir() -> code:priv_dir(?MODULE).

-spec reply(cowboy:http_status(), iodata() | {non_neg_integer() | cowboy_req:resp_body_fun()}, Req)
	-> {ok, Req} when Req::cowboy_req:req().
reply(Code, Body, Req) ->
    reply(Code, [], Body, Req).

-spec reply(cowboy:http_status(), cowboy:http_headers(),
	iodata() | {non_neg_integer() | cowboy_req:resp_body_fun()}, Req)
	-> {ok, Req} when Req::cowboy_req:req().
reply(Code, Headers, Body, Req) ->
    {'ok', Req1} = cowboy_req:reply(Code, Headers, Body, Req),
    {'halt', Req1}.

-spec path(cowboy_req:req()) -> ne_binaries().
path(Req) ->
    [X || X <- re:split(cowboy_req:get(['path'], Req), "/"), X =/= <<>>].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- ['crypto'
                                                ,'lager'
                                                ,'whistle_amqp'
                                                ,'whistle_couch'
                                               ]],
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    wapi_self:declare_exchanges().

maybe_start_http() ->

    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:info("HTTP autoprovisioner not enabled");
        'true' -> start_http()
    end.

start_http() ->
    Port = whapps_config:get_integer(?CONFIG_CAT, <<"port">>, 8001),
    ReqTimeout = whapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 1000),
    Workers = whapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 100),


    case catch cowboy:start_http({'stromboli', 'http'}
                                 ,Workers
                                 ,[{'port', Port}]
                                 ,[{'env', [#sboli_req{timeout = ReqTimeout}]}
                                   ,{'middlewares', ['sboli_authz'
                                                     ,'sboli_authn'
                                                     ,'sboli_router'
                                                    ]}
                                  ])
    of
        {'ok', _} -> lager:info("started HTTP autoprovisioner server");
        {'alredy_started', _} -> lager:info("already started HTTP autoprovisioner server");
        {'EXIT', Reason} -> lager:warning("crashed starting HTTP autoprovisioner server: ~p", [Reason])
    end.

maybe_start_https() -> 'ok'.
maybe_start_tftp() -> 'ok'.

