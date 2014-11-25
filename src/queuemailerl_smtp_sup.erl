%% @doc Supervisor for the many temporary workers sending emails.
-module(queuemailerl_smtp_sup).

-behaviour(supervisor).
-export([init/1]).

init([]) ->
    ChildSpec = {queuemailerl_smtp_worker, {queuemailerl_smtp_worker, start_link, []},
                 transient, 10, workier, [queuemailerl_smtp_worker]},
    {ok, {{simple_one_for_one, 10, 10}, [ChildSpec]}}.
