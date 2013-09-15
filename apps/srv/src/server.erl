-module(server).

-export([start/0, stop/0]).

start() ->
    ok = application:start(ranch),
    ok = application:start(srv),
    ok.

stop() ->
    ok = application:stop(srv),
    ok = application:stop(ranch),
    ok.
