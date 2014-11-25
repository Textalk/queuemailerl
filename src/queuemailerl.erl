%% @doc Application and supervisor
-module(queuemailerl).

%% public API
-export([start/0]).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

%% @doc Starts the application and all dependent applications.
start() ->
    application:ensure_all_started(queuemailerl, permanent).

%% @doc Application callback
start(_Type, _Args) ->
    %% Start our supervisor
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Application callback
stop(_State) ->
    ok.

%% @doc Supervisor callback
init([]) ->
    Procs = [
        {queuemailerl_ampq_mgr, {queuemailerl_ampq_mgr, start_link, []},
         permanent, 10, worker, [queuemailerl_ampq_mgr]},
        {queuemailerl_listener, {queuemailerl_listener, start_link, []},
         permanent, 10, worker, [queuemailerl_listener]},
        {queuemailer_smtp_sup, {queuemailer_smtp_sup, start_link, []},
         permanent, 10, supervisor, [queuemailer_smtp_sup]}
    ],
    {ok, {{one_for_all, 10, 10}, Procs}}.
