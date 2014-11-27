-module(queuemailerl_smtp_worker).

-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% The email type from gen_smtp_client.
-type email() :: {string() | binary(), [string() | binary(), ...], string() | binary() | function()}.

-record(state, {
          tag             :: binary(),                     %% The RabbitMQ tag on the event (used for ack)
          mail            :: email(),                      %% The email to be sent
          smtp            :: [{atom(), any()}],            %% The SMTP server to send it to
          retry_count = 0 :: non_neg_integer(),            %% Retries so far
          max_retries     :: non_neg_integer() | infinity, %% Maximum number of retries
          initial_delay   :: non_neg_integer()             %% The initial delay (increased exponentially)
         }).

%% --- Public API ---

%% @doc The Tag is identifying the message in the RabbitMQ queue so that we can
%% ack it when we are done.
-spec start_link(Tag :: term(), Event :: queuemailerl_event:event()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Tag, Event) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tag, Event], []).

%% --- Gen_server callbacks ---

init([Tag, Event]) ->
    State = #state{
               tag           = Tag,
               mail          = queuemailerl_event:build_mail(Event),
               smtp          = queuemailerl_event:get_smtp_options(Event),
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

handle_info(retry, State = #state{mail = Mail, smtp = Smtp, tag = Tag}) ->
    %% (Re-)try to send the email.
    case gen_smtp_client:send_blocking(Mail, Smtp) of
        Receipt when is_binary(Receipt) ->
            %% Successful. We got a receipt from the server.
            gen_server:cast(queuemailerl_listener, {ack, Tag}),
            {stop, normal, State};
        {error, Type, Message} ->
            error_logger:info_msg("Failed to send mail: ~p ~p", [Type, Message]),
            dispatch_retry(State);
        {error, Reason} ->
            error_logger:info_msg("Failed to send mail: ~p", [Reason]),
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
dispatch_retry(State) ->
    send_error_mail(State),
    %% Ack the event to RabbitMQ as we have done everything we could.
    gen_server:cast(queuemail_listener, {ack, State#state.tag}),
    {stop, normal, State}.

send_error_mail(State = #state{mail = Mail, smtp = Smtp}) ->
    {ok, ErrorSmtp} = application:get_env(queuemailerl, error_smtp),
    %% TODO: Build multipart mail.
    ok = todo.
