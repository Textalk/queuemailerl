-module(queuemailerl_smtp_worker).

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
          event           :: queuemailerl_event:event(),   %% The email to be sent, etc.
          retry_count = 0 :: non_neg_integer(),            %% Retries so far
          max_retries     :: non_neg_integer() | infinity, %% Maximum number of retries
          initial_delay   :: non_neg_integer()             %% The initial delay (increased exponentially)
         }).

%% --- Public API ---

%% @doc The Tag is identifying the message in the RabbitMQ queue so that we can
%% ack it when we are done.
-spec start_link(Event :: queuemailerl_event:event()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Event) ->
    gen_server:start_link(?MODULE, [Event], []).

%% --- Gen_server callbacks ---

init([Event]) ->
    State = #state{
               event         = Event,
               max_retries   = application:get_env(queuemailerl, retry_count, 10),
               initial_delay = application:get_env(queuemailerl, retry_initial_delay, 60000)
              },
    self() ! retry,
    {ok, State}.

%% @doc There are no recognized gen_server calls.
handle_call(_Call, _From, _State) ->
    error(badarg).

%% @doc There are no recognized gen_server casts.
handle_cast(_Cast, _State) ->
    error(badarg).

%% @doc
%% TODO: Save error Type and Message in the state and include them in the error
%% mail when we give up.
handle_info(retry, State = #state{event = Event}) ->
    %% (Re-)try to send the email.
    Mail = queuemailerl_event:get_mail(Event),
    Smtp = queuemailerl_event:get_smtp_options(Event),
    case gen_smtp_client:send_blocking(Mail, Smtp) of
        Receipt when is_binary(Receipt) ->
            %% Successful. We got a receipt from the server.
            {stop, normal, State};
        {error, no_more_hosts, {permanent_failure, _Host, _Message}} ->
            %% Permanent failure i.e wrong username etc. Drop them.
            send_error_mail(State),
            {stop, normal, State};
        {error, _Type, _Message} ->
            dispatch_retry(State);
        {error, _Reason} ->
            dispatch_retry(State)
    end;
handle_info(Info, State) ->
    %% Some other message to the server pid
    error_logger:info_msg("~p ignoring info ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- internal --

%% @doc Dispatch a retry for an email.
%% This will check and see if we've reached the maximum amount of retries. If the limit
%% is reached then we send an error email to the preconfigured error smtp server with the
%% failed email as an attachment. If we should try again then we increase the timeout
%% exponentially, send ourselves a retry-message and go into hibernation.
dispatch_retry(#state{retry_count = RetryCount,
                      max_retries = MaxRetries,
                      initial_delay = InitialDelay} = State)
  when RetryCount < MaxRetries ->
    %% Retry again after exponential back-off
    erlang:send_after(InitialDelay bsl RetryCount, self(), retry),
    {noreply, State#state{retry_count = RetryCount + 1}, hibernate};
dispatch_retry(State = #state{retry_count = RetryCount,
                              max_retries = MaxRetries})
  when RetryCount >= MaxRetries ->
    send_error_mail(State),
    %% Ack the event to RabbitMQ as we have done everything we could.
    {stop, normal, State}.

send_error_mail(#state{event = Event}) ->
    {ok, ErrorSmtp} = application:get_env(queuemailerl, error_smtp),
    %% Build multipart mail.
    ErrorMail = queuemailerl_event:build_error_mail(Event),
    case gen_smtp_client:send_blocking(ErrorMail, ErrorSmtp) of
        Receipt when is_binary(Receipt) ->
            %% Success. We got a receipt from the server.
            ok;
        {error, Type, Message} ->
            error_logger:error_msg("Failed to send error mail: ~p ~p", [Type, Message]);
        {error, Reason} ->
            error_logger:error_msg("Failed to send error mail: ~p", [Reason])
    end.
