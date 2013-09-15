-module(srv_protocol).
-behavior(gen_server).
-behavior(ranch_protocol).

-export([start_link/4]).
-export([init/2]).
-export([handle_info/2, terminate/2]).

-define(RETRY, 3).

-define(TIMEOUT, timer:hours(1)).

-record(state, {
          local = undefined,
          remote = undefined
         }).

start_link(Ref, Socket, _Transport, _Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket]).

init(Ref, Socket) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = inet:setopts(Socket, [binary, {active, once}, {packet, 4}, {reuseaddr, true}]),
    gen_server:enter_loop(?MODULE, [], #state{local=Socket}, ?TIMEOUT).

handle_info({tcp, Socket, <<5,1,0>>}, #state{local=Socket} = State) ->
    gen_tcp:send(Socket, <<5,0>>),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?TIMEOUT};

handle_info({tcp, Socket, <<5,1,0,Data/binary>>}, #state{local=Socket} = State) ->
    {RemoteAddr, RemotePort} = parse_addr_port(Data),
    case connect_remote(RemoteAddr, RemotePort, ?RETRY) of
        {ok, Remote} ->
            gen_tcp:send(Socket, <<5,0,0,Data/binary>>),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{remote=Remote}, ?TIMEOUT};
        {error, _Reason} ->
            lager:error("~p:[~w,~w]", [_Reason, RemoteAddr, RemotePort]),
            gen_tcp:send(Socket, <<5,1,0,Data/binary>>),
            {stop, normal, State}
    end;

handle_info({tcp, Socket, Data}, #state{local=Socket} = State) ->
    gen_tcp:send(State#state.remote, Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?TIMEOUT};

handle_info({tcp, Socket, Data}, #state{remote=Socket} = State) ->
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

parse_addr_port(<<1, Addr:4/binary, Port:16>>) ->
    {list_to_tuple(binary_to_list(Addr)), Port};
parse_addr_port(<<4, Addr:16/binary, Port:16>>) ->
    {list_to_tuple(binary_to_list(Addr)), Port};
parse_addr_port(<<3, Len, Addr:Len/binary, Port:16>>) ->
    {Addr, Port}.

connect_remote(_, _, 0) -> {error, cannot_connect_remote};
connect_remote(RemoteAddr, RemotePort, Retry) ->
    Opts = [{active, once}],
    case gen_tcp:connect(RemoteAddr, RemotePort, Opts, timer:seconds(30)) of
        {ok, Socket} -> {ok, Socket};
        {error, _}  -> connect_remote(RemoteAddr, RemotePort, Retry-1)
    end.
