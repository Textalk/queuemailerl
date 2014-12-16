%% @doc A gen_server wrapper for the AMPQ processes we need.
%% This is needed because the amqp_client-library doesn't handle this stuff in a supervisor friendly
%% way. This module starts a connection and a channel to the broker which we use to retrive email
%% events from the queue. If anything goes wrong here just shutdown or let it crash.
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
    {ok, RabbitConfigs} = application:get_env(queuemailerl, rabbitmq_configs),

    {ok, ConnectionPid} = connect(RabbitConfigs),
    monitor(process, ConnectionPid),

    {ok, ChannelPid} = amqp_connection:open_channel(ConnectionPid),
    monitor(process, ChannelPid),

    {ok, {ConnectionPid, ChannelPid}}.

handle_call(get_channel, _From, State = {_, ChannelPid}) ->
    {reply, ChannelPid, State};
handle_call(Call, _From, State) ->
    error_logger:info_msg("~p ignoring call ~p", [?MODULE, Call]),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    error_logger:info_msg("~p ignoring cast ~p", [?MODULE, Cast]),
    {noreply, State}.

handle_info({'DOWN', _MRef, process, _ConnOrChan, Reason}, State) ->
    %% The connection or channel to MQ has died.
    %% Stop and let the supervisor restart us.
    error_logger:error_msg("Connection or channel to RabbitMQ died.~nReason: ~p~n", [Reason]),
    {stop, normal, State};
handle_info(Info, State) ->
    %% Some other message to the server pid
    error_logger:info_msg("~p ignoring info ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, {ConnectionPid, ChannelPid}) ->
    amqp_channel:close(ChannelPid),
    amqp_connection:close(ConnectionPid, 1000).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --- Internal ---

%% Connect to any of the RabbitMQ servers
-spec connect([list()]) -> {ok, pid()}.
connect([MQParams | Rest]) ->
    AmqpConnParams = #amqp_params_network{
        username           = proplists:get_value(username, MQParams),
        password           = proplists:get_value(password, MQParams),
        virtual_host       = proplists:get_value(vhost, MQParams, <<"/">>),
        host               = proplists:get_value(host, MQParams, "localhost"),
        port               = proplists:get_value(port, MQParams, 5672),
        heartbeat          = 5,
        connection_timeout = 60000
    },
    case amqp_connection:start(AmqpConnParams) of
        {ok, Connection} ->
            {ok, Connection};
        {error, Reason} ->
            error_logger:warning_msg("Unable to connect to RabbitMQ broker"
                                     " for reason ~p using config ~p.",
                                     [Reason, MQParams]),
            connect(Rest)
    end;
connect([]) ->
    error_logger:error_msg("Failed to connect to all RabbitMQ brokers."),
    error(no_connection_to_mq).
