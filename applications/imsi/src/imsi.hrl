-ifndef(IMSI_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"imsi">>).
-define(APP_VERSION, <<"0.4.2">>).

-define(CONFIG_CAT, <<"imsi">>).

-record(auth_user, {realm
                    ,username
                    ,password
                    ,account_id
                    ,account_db
                    ,authorizing_type
                    ,authorizing_id
                    ,method
                    ,owner_id
                    ,suppress_unregister_notifications
                    ,register_overwrite_notify
                    ,account_realm
                    ,account_name
                    ,nonce
                   }).
-type auth_user() :: #auth_user{}.

-define(IMSI_HRL, 'true').
-endif.
