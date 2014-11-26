-module(test_smtp_server).
-behaviour(gen_smtp_server_session).

-export([start/1, stop/0]).

-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
         handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
         handle_other/3, handle_AUTH/4, handle_STARTTLS/1, code_change/3, terminate/2]).

-define(RELAY, true).

-record(state,
	{
		options = [] :: list()
	}).

-type(error_message() :: {'error', string(), #state{}}).

%% @doc Start a test smtp server that relays all incoming mails to the given process
start(Port) ->
    gen_smtp_server:start({local, test_smtp}, test_smtp_server, [[{test_pid, self()}, {port, Port}]]).

%% @doc Stop the test smtp server
stop() ->
    gen_smtp_server:stop(test_smtp).

%% @doc Initialize the callback module's state for a new session.
%% The arguments to the function are the SMTP server's hostname (for use in the SMTP anner),
%% The number of current sessions (eg. so you can do session limiting), the IP address of the
%% connecting client, and a freeform list of options for the module. The Options are extracted
%% from the `callbackoptions' parameter passed into the `gen_smtp_server_session' when it was
%% started.
%%
%% If you want to continue the session, return `{ok, Banner, State}' where Banner is the SMTP
%% banner to send to the client and State is the callback module's state. The State will be passed
%% to ALL subsequent calls to the callback module, so it can be used to keep track of the SMTP
%% session. You can also return `{stop, Reason, Message}' where the session will exit with Reason
%% and send Message to the client.
-spec init(Hostname :: binary(), SessionCount :: non_neg_integer(), Address :: tuple(), Options :: list()) -> {'ok', string(), #state{}} | {'stop', any(), string()}.
init(Hostname, _SessionCount, _Address, Options) ->
    Banner = [Hostname, " ESMTP test_smtp_server"],
    State = #state{options = Options},
    {ok, Banner, State}.

%% @doc Handle the HELO verb from the client. Arguments are the Hostname sent by the client as
%% part of the HELO and the callback State.
%%
%% Return values are `{ok, State}' to simply continue with a new state, `{ok, MessageSize, State}'
%% to continue with the SMTP session but to impose a maximum message size (which you can determine
%% , for example, by looking at the IP address passed in to the init function) and the new callback
%% state. You can reject the HELO by returning `{error, Message, State}' and the Message will be
%% sent back to the client. The reject message MUST contain the SMTP status code, eg. 554.
-spec handle_HELO(Hostname :: binary(), State :: #state{}) -> {'ok', pos_integer(), #state{}} | {'ok', #state{}} | error_message().
handle_HELO(_Hostname, State) ->
    {ok, State}.

%% @doc Handle the EHLO verb from the client. As with EHLO the hostname is provided as an argument,
%% but in addition to that the list of ESMTP Extensions enabled in the session is passed. This list
%% of extensions can be modified by the callback module to add/remove extensions.
%%
%% The return values are `{ok, Extensions, State}' where Extensions is the new list of extensions
%% to use for this session or `{error, Message, State}' where Message is the reject message as
%% with handle_HELO.
-spec handle_EHLO(Hostname :: binary(), Extensions :: list(), State :: #state{}) -> {'ok', list(), #state{}} | error_message().
handle_EHLO(_Hostname, Extensions, State) ->
    {ok, Extensions, State}.

%% @doc Handle the MAIL FROM verb. The From argument is the email address specified by the
%% MAIL FROM command. Extensions to the MAIL verb are handled by the `handle_MAIL_extension'
%% function.
%%
%% Return values are either `{ok, State}' or `{error, Message, State}' as before.
-spec handle_MAIL(From :: binary(), State :: #state{}) -> {'ok', #state{}} | error_message().
handle_MAIL(_From, State) ->
    {ok, State}.

%% @doc Handle an extension to the MAIL verb. Return either `{ok, State}' or `error' to reject
%% the option.
-spec handle_MAIL_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_MAIL_extension(_Extension, State) ->
    {ok, State}.

-spec handle_RCPT(To :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_RCPT(_To, State) ->
    {ok, State}.

-spec handle_RCPT_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_RCPT_extension(_Extension, State) ->
    {ok, State}.

-spec handle_DATA(From :: binary(), To :: [binary(),...], Data :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_DATA(From, To, Data, #state{options = Options} = State) ->
    TestPid = proplists:get_value(test_pid, Options),
    TestPid ! {From, To, Data},
    {ok, "Ok", State}.

-spec handle_RSET(State :: #state{}) -> #state{}.
handle_RSET(State) ->
    State.

-spec handle_VRFY(Address :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_VRFY(_Address, State) ->
    {ok, "Ok", State}.

-spec handle_other(Verb :: binary(), Args :: binary(), #state{}) -> {string(), #state{}}.
handle_other(Verb, _Args, State) ->
    {["500 Error: command not recognized : '", Verb, "'"], State}.

%% this callback is OPTIONAL
%% it only gets called if you add AUTH to your ESMTP extensions
-spec handle_AUTH(Type :: 'login' | 'plain' | 'cram-md5', Username :: binary(), Password :: binary() | {binary(), binary()}, #state{}) -> {'ok', #state{}} | 'error'.
handle_AUTH(_Type, _Username, _Password, _State) ->
    error.

%% this callback is OPTIONAL
%% it only gets called if you add STARTTLS to your ESMTP extensions
-spec handle_STARTTLS(#state{}) -> #state{}.
handle_STARTTLS(State) ->
    State.

-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}.
terminate(Reason, State) ->
    {ok, Reason, State}.
