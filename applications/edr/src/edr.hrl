-ifndef(EDR_HRL).
-define(EDR_HRL, 'true').

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"edr">>).
-define(APP_VERSION, <<"0.0.1">>).
-record(backend, {tags          :: wh_json:object()
                  ,name         :: ne_binary()
                  ,type         :: ne_binary()
                  ,enabled      :: boolean()
                  ,options      :: wh_json:object()
                 }).
-type backend() :: #backend{}.
-type work_result() :: 'ok' | {'error', Info :: any()} | {'exit', Reason ::any()}.
-endif.
