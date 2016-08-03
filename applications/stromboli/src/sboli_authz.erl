-module(sboli_authz).

-export([execute/2]).

-include("stromboli.hrl").

-spec execute(cowboy_req:req(), any()) -> {'ok', cowboy_req:req(), any()}
                                          | {'error', cowboy:http_status(), cowboy_req:req()}.
execute(Req, State) ->
    {'ok', Req, State}.
