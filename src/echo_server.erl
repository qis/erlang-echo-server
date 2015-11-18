-module(echo_server).

-behaviour(gen_server).

%% API.

-export([start_link/1]).
-export([initialise/1]).

%% gen_server.

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {lsock}).

%% API.

%% echo_server:initialise(5).

initialise(N) ->
    [echoserver_sup:spawn_child() || _ <- lists:seq(1, N)].
    
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%% gen_server.

init([LSock]) ->
    io:format("server ~p started~n", [self()]),
    {ok, #state{lsock=LSock}, 0}. %% NB timeout 0

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{lsock=LSock}=State) ->
    {ok, Socket}=gen_tcp:accept(LSock),
    inet:setopts(Socket, [{active, once}]),
    io:format("~p accepted~n", [Socket]),
    echoserver_sup:spawn_child(),
    {noreply, State};
handle_info({tcp, Socket, RawData}, State) ->
    io:format("Received ~p from ~p~n", [RawData, Socket]),
    inet:setopts(Socket, [{active, once}]), 
    gen_tcp:send(Socket, io_lib:fwrite("You sent: ~p~n", [RawData])),
    {noreply, State};
handle_info({tcp_closed, Socket}, State) ->
    io:format("~p closed~n", [Socket]),
    {stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
    io:format("~p error; stopping~n", [Socket]),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p terminated~n", [self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
