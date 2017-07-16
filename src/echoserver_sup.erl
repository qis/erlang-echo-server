-module(echoserver_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([spawn_child/0]).

-export([init/1]).

-define(PORT, 8080).

-define(N_PROCS, 5).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [?PORT]).

spawn_child() ->
    supervisor:start_child(?MODULE, []). %% NB simple_one_for_one ie no (extra) args

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}]),
    WorkerSpec={echo_server, 
      {echo_server, start_link, [LSock]},
      temporary, 
      brutal_kill, 
      worker, 
      [echo_server]
    },
    Procs = [WorkerSpec],
    spawn(fun() -> echo_server:initialise(?N_PROCS) end), %% NB needs to be in a separate process
    {ok, {{simple_one_for_one, 1, 5}, Procs}}. %% NB simple_one_for_one
