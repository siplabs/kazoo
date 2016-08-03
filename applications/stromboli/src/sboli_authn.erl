-module(sboli_authn).

-export([execute/2]).

-include("stromboli.hrl").
-define(UNITS_DB,<<"provision_units">>).
-define(PHONE_CONFIGS_DB,<<"phone_configs">>).
-define(LIST_BY_IP,<<"stromboli/list_by_ip">>).

-spec execute(cowboy_req:req(), stromboli:state()) ->
                {'ok', cowboy_req:req(), any()}
                | {'error', cowboy:http_status(), cowboy_req:req()}
                | {'halt', cowboy_req:req()}.
execute(Req, [_, State]) ->
    lager:debug([{trace, true}], "State is ~p", [State]),
    case authn('mac', Req, State) of
        'unauthorized' ->
            stromboli:reply(403, "Authentication failed", Req);
        Reply -> Reply
end.

-spec authn(stromboli:mac(), cowboy_req:req(), stromboli:state()) ->
            'false'
            | {'ok', cowboy_req:req()}
            | {'ok', cowboy_req:req(), stromboli:state()}.
authn('mac', Req, State) ->
    lager:debug([{trace, true}], "state ~p", [State]),
    case mac_from_request(Req) of
        {'error', 'not_found'} ->
            lager:warning("can't resolve MAC from path ~s", [stromboli:path(Req)]),
            'unauthorized';
        {'ok', Mac} ->
            {Host, Req1} = cowboy_req:host(Req),
            lager:debug([{trace, true}], "Host is: ~p", [Host]),
            case find_unit(Mac, Host, State) of
                {'ok', State1} ->
                    lager:debug([{trace, true}], "Unit ID is ~p", [State1#sboli_req.unit_id]),
                    {'ok', Req1, State1};
                {'error', _} ->
                    lager:debug([{trace, true}], "UnitId for mac ~p is not found", [Mac]),
                    'unauthorized'
            end
    end;

authn(What, Req, _) ->
    lager:error("authentication failed: unknown method ~p", [What]),
    stromboli:reply(500, "", Req).

-spec mac_from_request(cowboy_req:req()) -> stromboli:mac().
mac_from_request(Req) ->
    case stromboli:path(Req) of
        [] -> {'error', 'not_found'};
        Path -> sboli_polycom:try_get_mac(lists:last(Path))
    end.

-spec find_unit(stromboli:mac(), binary(), stromboli:state()) ->
                    {'ok', stromboli:state()} | {'error', 'not_found'} .
find_unit(Mac, Realm, State) ->
    case whapps_util:get_account_by_realm(Realm) of
        {'ok', AccId} ->
            case sboli_unit:get_unit_by_mac(Mac, AccId) of
                {'ok', UnitId} ->
                    {'ok', State#sboli_req{account_id = AccId, unit_id = UnitId}};
                E -> E
            end;
        _ -> {'error', 'not_found'}
    end.
