%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(sboli_router).

-export([execute/2]).

-include("stromboli.hrl").
-define(UNITS_DB,<<"provision_units">>).

-spec execute(cowboy_req:req(), stromboli:state()) ->
                {'ok', cowboy_req:req(), stromboli:state()}
                | {'error', cowboy:http_status(), cowboy_req:req()}
                | {'halt', cowboy_req:req()}.
execute(Req, State) ->
  case stromboli:path(Req) of
    [] ->
      route('root', Req, State);
    _ ->
        {Req1, State1} = sboli_general:gen_conf(Req, State),
        route('config', Req1, State1)
  end.

-spec route('atom', cowboy_req:req(), stromboli:state()) ->
                {'ok', cowboy_req:req(), stromboli:state()}
                | {'error', cowboy:http_status(), cowboy_req:req()}
                | {'halt', cowboy_req:req()}.
route('root', Req, State) ->
  stromboli:reply(200, <<"Welcome to stromboli provisioner\r\n"
                  ,(State#sboli_req.unit_id )/binary>>, Req);
route('config', Req, State) ->
    lager:debug([{trace, true}],"Body is ~p", [State#sboli_req.body]),
    stromboli:reply(200, State#sboli_req.body, Req).




