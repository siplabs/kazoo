-ifndef(STROMBOLI_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(CONFIG_CAT, <<"stromboli">>).

-define(APP_NAME, <<"stromboli">>).
-define(APP_VERSION, <<"0.0.1">> ).

-define(STROMBOLI_HRL, 'true').

-record(sboli_req, {timeout
                    ,account_id
                    ,model
                    ,unit_id
                    ,unit_json
                    ,body
                   }).

-record(model, {name
               ,family
               ,vendor
               }).

-type state()::#sboli_req{}.
-type mac()::<<_:12>>.
-export_type([state/0
             ,mac/0
             ]).

-endif.
