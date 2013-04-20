-module(freesock_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Acceptors} = application:get_env(freesock, acceptors),
    {ok, ListenPort} = application:get_env(freesock, listen_port),
    {ok, _} = ranch:start_listener(freesock, Acceptors,
                                   ranch_tcp, [{port, ListenPort}],
                                   socks5_protocol, [{active, true}]),
    freesock_sup:start_link().

stop(_State) ->
    ok.

listen_opts() ->
    {ok, Opts} = application:get_env(freesock, listen_opts),
    Opts.
