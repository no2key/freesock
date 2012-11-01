-module(echo).
-behaviour (gen_server).

-export ([accept/1, loop/1, transfer_data/2]).
-export ([start_link/0]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, [], 0}.

handle_info(timeout, State) ->
    {ok, Port} = application:get_env(fs, port),
    {ok, LSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    lists:foreach(
        fun(_)->
            proc_lib:spawn_link(?MODULE, accept, [LSock])
        end, lists:duplicate(erlang:system_info(schedulers), dump)),
    {noreply, State}.

accept(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = proc_lib:spawn_link(?MODULE, loop, [Sock]),
    gen_tcp:controlling_process(Sock, Pid),
    accept(LSock).

% connection request
loop(Sock) ->
    receive
        {tcp, Sock, <<5, _, _>>} ->
            % initial greeting
            gen_tcp:send(Sock, <<5, 0>>),
            loop(Sock);
        {tcp, Sock, <<5, 1, 0, Data/binary>>} ->
            % connection request
            RSock =
                case Data of
                    <<3, LenDomain, Domain:LenDomain/binary, Port:16>> ->
                        connect_server(binary_to_list(Domain), Port);
                    <<1, IPv4:32, Port:16>> ->
                        connect_server(binary_to_list(IPv4), Port);
                    <<4, IPv6:128, Port:16>> ->
                        connect_server(binary_to_list(IPv6), Port);
                    _Other ->
                         % unexpected packages
                        gen_tcp:close(Sock)
                end,
            case RSock of
                undefined ->
                    gen_tcp:send(Sock, <<5, 1, 0, Data/binary>>),
                    gen_tcp:close(Sock);
                _ ->
                    gen_tcp:send(Sock, <<5, 0, 0, Data/binary>>),
                    transfer_data(Sock, RSock)
            end
    end.

% connect to real server
connect_server(Address, Port) ->
    case gen_tcp:connect(Address, Port, [binary, {packet, 0}]) of
        {ok, RSock} -> RSock;
        _Error -> undefined
    end.

transfer_data(Sock, RSock) ->
    receive
        {tcp, Sock, Data} ->
            gen_tcp:send(RSock, Data),
            transfer_data(Sock, RSock);
        {tcp, RSock, Data} ->
            gen_tcp:send(Sock, Data),
            transfer_data(Sock, RSock);
        {tcp_closed, Sock}->
            gen_tcp:close(Sock);
        {tcp_closed, RSock}->
            gen_tcp:close(RSock);
        {tcp_error, Sock, _Reason} ->
            gen_tcp:close(Sock);
        {tcp_error, RSock, _Reason} ->
            gen_tcp:close(RSock)
    end.

handle_call(_R, _F, State) ->
    {ok, [], State}.

handle_cast(_R, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


