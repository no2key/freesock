-module(socks5_protocol).
-behavior(ranch_protocol).
-export([start_link/4]).
-export([init/4]).

-define(TIMEOUT, 20000).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    ok = ranch:accept_ack(Ref),
    lager:info("listen_opts:~p", [inet:getopts(Socket, [packet, active])]),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    {OK, Closed, Error} = Transport:messages(),
    Transport:setopts(Socket, [{active, once}]),
    receive
        %% identify socks version
        {OK, Socket, <<5, _, _>>} ->
            lager:info("identify socks version"),
            %% No authentication required
            Ack = <<5, 0>>,
            Transport:send(Socket, Ack),
            loop(Socket, Transport);
        %% receive request details
        {OK, Socket, <<5, 1, _, DstInfo/binary>>} ->
            lager:info("receive request details"),
            case get_real_dst(DstInfo) of
                unknown -> Transport:close(Socket);
                {DstAddr, DstPort} ->
                    %% Connect to server
                    case connect(DstAddr, DstPort) of
                        {ok, DstSocket} ->
                            Ack = <<5, 0, 0, DstInfo/binary>>,
                            Transport:send(Socket, Ack),
                            start_loop(Socket, Transport, DstSocket);
                        error ->
                            Ack = <<5, 1, 0, DstInfo/binary>>,
                            Transport:send(Socket, Ack),
                            Transport:close(Socket)
                    end
            end;
        _Unknown ->
            lager:info("Unknown:~p", [_Unknown]),
            ok = Transport:close(Socket)
    end.

%% helper methods

%% Get real addr and port to connect
get_real_dst(<<1, A, B, C, D, Port:16>>) ->
    IPv4 = inet_parse:ntoa({A, B, C, D}),
    {IPv4, Port};
get_real_dst(<<3, Len:8, Domain:Len/binary, Port:16>>) -> {Domain, Port};
get_real_dst(<<4, IPv6:128, Port:16>>) -> {IPv6, Port};
get_real_dst(_Unknown) ->
    lager:error("Unknown DstInfo:~p", [_Unknown]),
    unknown.

%% Connect to destination server
connect(DstAddr, DstPort) ->
    case gen_tcp:connect(DstAddr, DstPort, [binary, {packet, raw}]) of
        {ok, Socket} -> {ok, Socket};
        {error, _Reason} ->
            lager:error("Connect remote server [~p:~p] error:~p",
                        [DstAddr, DstPort, _Reason]),
            error
    end.

%% Tranfer user data
start_loop(Socket, Transport, DstSocket) ->
    loop_socket(Socket, Transport, DstSocket),
    ok.

loop_socket(Socket, Transport, DstSocket) ->
    {OK, Closed, Error} = Transport:messages(),
    Transport:setopts(Socket, [{active, once}]),
    receive
        {OK, Socket, Data} ->
            gen_tcp:send(DstSocket, Data),
            loop_socket(Socket, Transport, DstSocket);
        {Closed, Socket} ->
            lager:info("socket closed");
        {Error, Socket, _Reason} ->
            lager:error("Error loop socket:~p", [_Reason]);
        {tcp, DstSocket, Data} ->
            Transport:send(Socket, Data),
            loop_socket(Socket, Transport, DstSocket);
        {tcp_error, DstSocket, _Reason} ->
            lager:error("Error loop dst_socket:~p", [_Reason]);
        {tcp_closed, DstSocket} ->
            ok
    end.
%% case Transport:recv(Socket, 0, ?TIMEOUT) of
%%     {ok, Data} ->
%%         gen_tcp:send(DstSocket, Data),
%%         loop_socket(Transport, Socket, DstSocket);
%%     {error, _Reason} ->
%%         lager:error("Error loop socket:~p", [_Reason])
%% end.

loop_dst_socket(Socket, Transport, DstSocket) ->
    receive
        {tcp, DstSocket, Data} ->
            Transport:send(Socket, Data),
            loop_dst_socket(DstSocket, Transport, Socket);
        {tcp_error, DstSocket, _Reason} ->
            lager:error("Error loop dst_socket:~p", [_Reason]);
        {tcp_closed, DstSocket} ->
            ok
    end.
