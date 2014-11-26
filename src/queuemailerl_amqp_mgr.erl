%% @doc A gen_server wrapper for the AMPQ processes we need, because the amqp client doesn't handle
%% stuff in a supervisor friendly way.
-module(queuemailerl_amqp_mgr).

-export([start_link/0, get_channel/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

%% --- Public API ---

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns the RabbitMQ channel pid.
get_channel() ->
    gen_server:call(?MODULE, get_channel).

%% --- Gen_server callbacks ---

init([]) ->
    {ok, RabbitProps} = application:get_env(queuemailerl, rabbitmq),
    AmqpConnParams = #amqp_params_network{
        username           = proplists:get_value(username, RabbitProps),
        password           = proplists:get_value(password, RabbitProps),
        virtual_host       = proplists:get_value(vhost, RabbitProps, <<"/">>),
        host               = proplists:get_value(host, RabbitProps, "localhost"),
        port               = proplists:get_value(port, RabbitProps, 5672),
        heartbeat          = 5,
        connection_timeout = 60000
    },

    {ok, ConnectionPid} = amqp_connection:start(AmqpConnParams),
    monitor(process, ConnectionPid),

    {ok, ChannelPid} = amqp_connection:open_channel(ConnectionPid),
    monitor(process, ChannelPid),

    {ok, {ConnectionPid, ChannelPid}}.

handle_call(get_channel, _From, State = {_, ChannelPid}) ->
    {reply, ChannelPid, State}.

handle_cast(Cast, State) ->
    error_logger:info_msg("~p ignoring cast ~p", [?MODULE, Cast]),
    {noreply, State}.

handle_info({'DOWN', _MRef, process, _ConnOrChan, Reason}, State) ->
    %% The connection or channel to MQ has died.
    %% Stop and let the supervisor restart us.
    error_logger:error_msg("Connection or channel to RabbitMQ died.~nReason: ~p~n", [Reason]),
    {stop, normal, State};
handle_info({'EXIT', _Worker, normal}, State) ->
    %% A channel or connection has finished and exits gracefully.
    %% This is probably part of a shutdown procedure. Do nothing.
    {noreply, State};
handle_info({'EXIT', _Worker, Reason}, State) ->
    %% A worker has died before finishing the job, requeue the event
    %% Stop and let the supervisor restart us.
    error_logger:error_msg("Connection or channel to RabbitMQ died.~nReason: ~p~n", [Reason]),
    {stop, normal, State};
handle_info(Info, State) ->
    %% Some other message to the server pid
    error_logger:info_msg("~p ignoring info ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, {ConnectionPid, ChannelPid}) ->
    amqp_channel:close(ChannelPid),
    amqp_connection:close(ConnectionPid, 1000),
    %% close and/or kill the channel as well?
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
