%%%-------------------------------------------------------------------
%%% @author ivan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2016 12:31
%%%-------------------------------------------------------------------
-module(cb_account_provision_test).
-author("ivan").

-include_lib("eunit/include/eunit.hrl").
-define(ACCOUNT_ID,<<"88845d60770b19012fbecbbe0eff1f38">>).

simple_test() ->
    ?assert(true).

does_config_exists_test_() ->
    Context =
        cb_context:setters(
            cb_context:new()
            ,[{fun cb_context:set_account_db/2, wh_util:format_account_db(?ACCOUNT_ID)}
              ,{fun cb_context:set_resp_status/2, 'success'}
            ]
        ),
    [{<<"Verefy does_config_exists/1 returns true if provision config exists">>
       ,?_assertEqual('true', cb_account_provision:does_config_exists(Context))
     }
    ].