-module(srv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(socks_server, 100,
                                   ranch_tcp, [{port, 7070}],
                                   srv_protocol, []),
    srv_sup:start_link().

stop(_State) ->
    ok.
