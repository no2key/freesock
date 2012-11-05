-module(echo).
-behaviour (gen_server).

-export ([start_link/0]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================================================================
%% api
%% ===================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, [], 0}.

handle_info(timeout, State) ->
    {ok, Port} = application:get_env(fs, port),
    {ok, TcpOptions} = application:get_env(fs, tcp_options),
    {ok, SslOptions} = application:get_env(fs, ssl_options),
    {ok, LSock} = gen_tcp:listen(Port, TcpOptions),
    lists:foreach(
        fun(_)->
            supervisor:start_child(tcp_acceptor_sup, [LSock, SslOptions])
        end, lists:duplicate(erlang:system_info(schedulers), dump)),
    {noreply, State}.

handle_call(_R, _F, State) ->
    {ok, [], State}.

handle_cast(_R, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

