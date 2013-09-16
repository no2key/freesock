-module(cli_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(socks_client, 100,
                                   ranch_tcp, [{port, 7000}],
                                   cli_protocol, []),
    cli_sup:start_link().

stop(_State) ->
    ok.
