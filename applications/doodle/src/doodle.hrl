-ifndef(DOODLE_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").
-include_lib("whistle_apps/src/whapps_call_command_types.hrl").

-define(APP_NAME, <<"doodle">>).
-define(APP_VERSION, <<"0.0.1">> ).
-define(CONFIG_CAT, <<"doodle">>).

-define(DOODLE_CACHE, 'doodle_cache').

-define(CCV(Key), [<<"Custom-Channel-Vars">>, Key]).

-define(DOODLE_HRL, 'true').
-endif.
