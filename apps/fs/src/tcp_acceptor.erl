-module(tcp_acceptor).
-behaviour (gen_server).

-export ([start_link/2]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([loop/1]).

-record (state, {listen_sock, ssl_options}).

%% ===================================================================
%% api
%% ===================================================================
start_link(LSock, SslOptions) ->
    gen_server:start_link(?MODULE, [LSock, SslOptions], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([LSock, SslOptions]) ->
    State = #state{listen_sock = LSock, ssl_options = SslOptions},
    {ok, State, 0}.

handle_cast(_R, State) ->
    {noreply, State}.

handle_call(_R, _F, State) ->
    {ok, [], State}.

handle_info(timeout, State) ->
    accept(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% private functions
%% ===================================================================
accept(State) ->
    #state{listen_sock = LSock, ssl_options = SslOptions} = State,
    {ok, Sock} = gen_tcp:accept(LSock),
    inet:setopts(Sock, [{active, false}]),
    {ok, SslSock} = ssl:ssl_accept(Sock, SslOptions),
    Pid = proc_lib:spawn_link(?MODULE, loop, [SslSock]),
    ssl:controlling_process(SslSock, Pid),
    accept(State).

% connection request
loop(Sock) ->
    ssl:setopts(Sock, [{active, true}]),
    receive
        {ssl, Sock, <<5, _, _>>} ->
            % initial greeting
            ssl:send(Sock, <<5, 0>>),
            loop(Sock);
        {ssl, Sock, <<5, 1, 0, Data/binary>>} ->
            % connection request
            RSock =
                case Data of
                    <<3, LenDomain, Domain:LenDomain/binary, Port:16>> ->
                        connect_server(binary_to_list(Domain), Port);
                    <<1, IPv4:32, Port:16>> ->
                        connect_server(binary_to_list(IPv4), Port);
                    <<4, IPv6:128, Port:16>> ->
                        connect_server(binary_to_list(IPv6), Port);
                    Other ->
                         % unexpected packages
                        error_logger:error_msg("Other:~p~n", [Other]),
                        ssl:close(Sock)
                end,
            case RSock of
                undefined ->
                    ssl:send(Sock, <<5, 1, 0, Data/binary>>),
                    ssl:close(Sock);
                _ ->
                    ssl:send(Sock, <<5, 0, 0, Data/binary>>),
                    transfer_data(Sock, RSock)
            end;
        Other ->
            % unexpected packages
            error_logger:error_msg("Other:~p~n", [Other]),
            ssl:close(Sock)
    end.

% connect to real server
connect_server(Address, Port) ->
    case gen_tcp:connect(Address, Port, [binary, {packet, 0}]) of
        {ok, RSock} -> RSock;
        _Error -> undefined
    end.

transfer_data(Sock, RSock) ->
    receive
        {ssl, Sock, Data} ->
            gen_tcp:send(RSock, Data),
            transfer_data(Sock, RSock);
        {tcp, RSock, Data} ->
            ssl:send(Sock, Data),
            transfer_data(Sock, RSock);
        {ssl_error, Sock, _Reason} ->
            ssl:close(Sock);
        {tcp_error, RSock, _Reason} ->
            gen_tcp:close(RSock);
        {ssl_closed, Sock} ->
            ok;
        {tcp_closed, RSock} ->
            ok
    end.
