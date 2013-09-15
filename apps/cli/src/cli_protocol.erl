-module(cli_protocol).
-behavior(gen_server).
-behavior(ranch_protocol).

-export([start_link/4]).
-export([init/2]).
-export([handle_info/2, terminate/2]).

-define(PROXY_ADDR, "127.0.0.1").
-define(PROXY_PORT, 7070).
-define(RETRY, 3).

-define(TIMEOUT, timer:hours(1)).

-record(state, {
          local = undefined,
          proxy = undefined
         }).

start_link(Ref, Socket, _Transport, _Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket]).

init(Ref, Socket) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = inet:setopts(Socket, [{active, once}]),
    case connect_proxy(?RETRY) of
        {ok, Proxy} -> 
            State = #state{local=Socket, proxy=Proxy},
            gen_server:enter_loop(?MODULE, [], State, ?TIMEOUT);
        {error, _Reason} ->
            lager:error("~p", [_Reason])
    end.

handle_info({tcp, Socket, Data}, #state{local=Socket} = State) ->
    gen_tcp:send(State#state.proxy, Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?TIMEOUT};

handle_info({tcp, Socket, Data}, #state{proxy=Socket} = State) ->
    gen_tcp:send(State#state.local, Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?TIMEOUT};

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    lager:info("tcp_error:~p", [Reason]),
    {stop, Reason, State};

handle_info(timeout, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

connect_proxy(0) -> {error, cannot_connect_proxy};
connect_proxy(Retry) ->
    Opts = [binary, {active, once}, {reuseaddr, true}],
    case gen_tcp:connect(?PROXY_ADDR, ?PROXY_PORT, Opts) of
        {ok, Socket} -> {ok, Socket};
        {error, _}  -> connect_proxy(Retry-1)
    end.
