-module(queuemailerl_smtp_worker).

-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tag, mail, smtp, retry_count = 0}).

%% --- Public API ---

%% @doc The Tag is identifying the message in the RabbitMQ queue so that we can
%% ack it when we are done.
-spec start_link(Tag :: term(), Event :: queuemailerl_event:event()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Tag, Event) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tag, Event], []).

%% --- Gen_server callbacks ---

init([Tag, Event]) ->
    Mail = queuemailerl_event:build_mail(Event),
    Smtp = queuemailerl_event:get_smtp_options(Event),
    State = #state{tag = Tag, mail = Mail, smtp = Smtp},
    {ok, State, 0}.

%% @doc There are no recognized gen_server calls.
handle_call(_Call, _From, _State) ->
    error(badarg).

%% @doc There are no recognized gen_server casts.
handle_cast(_Cast, _State) ->
    error(badarg).

handle_info(timeout, State = #state{mail = Mail, smtp = Smtp, tag = Tag}) ->
    %% (Re-)try to send the email.
    case gen_smtp_client:send_blocking(Mail, Smtp) of
        Receipt when is_binary(Receipt) ->
            %% Successful. We got a receipt from the server.
            gen_server:cast(queuemail_listener, {ack, Tag}),
            {stop, normal, State};
        {error, Type, Message} ->
            error_logger:info_msg("Failed to send mail: ~p ~s", [Type, Message]),
            dispatch_retry(State);
        {error, Reason} ->
            error_logger:info_msg("Failed to send mail: ~p", [Reason]),
            dispatch_retry(State)
    end;
handle_info(Info, State) ->
    %% Some other message to the server pid
    error_logger:info_msg("~p ignoring info ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- internal --

%% Return the same as handle_info. Its return value will be used for this.
dispatch_retry(State = #state{retry_count = RetryCount}) ->
    {ok, MaxRetries} = application:get_env(queuemailerl, retry_count),
    if
        RetryCount < MaxRetries ->
            {ok, InitialDelay} = application:get_env(queuemailerl, retry_initial_delay),
            Delay = InitialDelay bsl RetryCount,
            {noreply, State#state{retry_count = RetryCount + 1}, Delay};
        RetryCount >= MaxRetries ->
            send_error_mail(State),
            %% Ack as we have done everything we could.
            gen_server:cast(queuemail_listener, {ack, State#state.tag}),
            {stop, normal}
    end.

send_error_mail(State = #state{mail = Mail, smtp = Smtp}) ->
    {ok, ErrorSmtp} = application:get_env(queuemailerl, error_smtp),
    %% TODO: Build multipart mail.
    ok = todo.
