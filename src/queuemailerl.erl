%% @doc This module servers as both application and supervisor.
%%
%% The application has the following structure.
%%
%% <ul>
%%   <li>
%%     `queuemailerl': application module and top level `all_for_one' supervisor,
%%     supervising the following registered processes:
%%     <ul>
%%       <li>`queuemailerl_listener': A gen_server subscribing to a RabbitMQ
%%           queue. When a message arrives and starting
%%           `queuemailerl_smtp_worker' jobs.</li>
%%       <li>`queuemailer_smtp_sup': A simple_one_for_one supervisor for
%%           resilient `queuemailerl_smtp_worker' jobs:
%%           <ul><li>`queuemailerl_smtp_worker': A gen_server for sending an
%%                   SMTP email, waiting and retrying and – if all retries
%%                   fail – sending an error report email.</li></ul></li>
%%     </ul>
%%   </li>
%%   <li>`queuemailerl_event': A module for working with the abstract datatype
%%       `event()'. It is used for parsing an incoming JSON message and
%%       information from it.</li>
%% </ul>
-module(queuemailerl).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

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
        {queuemailerl_smtp_sup, {queuemailerl_smtp_sup, start_link, []},
         permanent, 10, supervisor, [queuemailerl_smtp_sup]},
        {queuemailerl_listener, {queuemailerl_listener, start_link, []},
         permanent, 10, worker, [queuemailerl_listener]}
    ],
    {ok, {{one_for_all, 10, 10}, Procs}}.
