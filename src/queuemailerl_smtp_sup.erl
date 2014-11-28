%% @doc Supervisor for the many temporary workers sending emails.
-module(queuemailerl_smtp_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).

%% @doc Start the process to supervise all temporary email workers
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = {queuemailerl_smtp_worker, {queuemailerl_smtp_worker, start_link, []},
                 transient, 10, worker, [queuemailerl_smtp_worker]},
    {ok, {{simple_one_for_one, 1, 10}, [ChildSpec]}}.
