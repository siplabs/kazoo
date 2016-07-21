%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% EDR in-core API
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(kz_edr).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-export([log_event/5]).

-type log_event_return() :: 'ok' |
                            {'error', any()} |
                            {'returned', wh_json:object(), wh_json:object()}.

-spec log_event(binary(), binary(), wh_json:object(), binary(), binary()) -> log_event_return().
log_event(EventCategory, EventName, Tags, AppName, AppVersion) ->
    Timestamp = wh_util:now_s(wh_util:now()),
    Req = [{<<"Timestamp">>, Timestamp}
           ,{<<"Tags">>, Tags}
           | wh_api:default_headers(EventCategory, EventName, AppName, AppVersion)
          ],
    wh_amqp_worker:cast(Req, fun wapi_edr:publish/1).
