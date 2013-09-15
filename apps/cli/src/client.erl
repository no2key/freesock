-module(client).

-export([start/0, stop/0]).

start() ->
    ok = application:start(ranch),
    ok = application:start(cli),
    ok.

stop() ->
    ok = application:stop(cli),
    ok = application:stop(ranch),
    ok.
