%% @doc A gen_server that listens for events from a RabbitMQ-queue.
%% It connects to RabbitMQ asynchronusly and starts to subscribe for mail events from
%% a specified queue. If something goes wrong then this process will tear down everything
%% and start from scratch.
-module(queuemailerl_listener).

-export([start_link/0]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(TIMEOUT, 10000).

%% --- Public API ---

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% --- Gen_server callbacks ---

init([]) ->
    self() ! connect,
    {ok, undefined}.

handle_call(_Call, _From, State) ->
    error(badarg).

%% @doc When a worker is done, it should cast an {ack, Tag} back to us.
handle_cast({ack, Tag}, {_, ChannelPid} = State) ->
    Acknowledge = #'basic.ack'{delivery_tag = Tag},
    amqp_channel:cast(ChannelPid, Acknowledge),
    {noreply, State}.

%% @doc Handles incoming messages from RabbitMQ.
handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}},
            {_, ChannelPid} = State) ->
    case queuemailerl_event:parse(Payload) of
        {ok, Event} ->
            supervisor:start_child(queuemailerl_smtp_sup, [Tag, Event]);
        {error, Reason} ->
            %% Error when parsing the event, log it then ack it to remove it from the queue
            error_logger:error_msg("Invalid queuemailerl message.~n"
                                   "Payload: ~p~n"
                                   "Reason: ~p~n"
                                   "Trace: ~p~n",
                                   [Payload, Reason, erlang:get_stacktrace()]),

            %% Since we consider the message invalid if this happens
            %% an ack is sent so that the message will be dropped from
            %% the queue.
            Acknowledge = #'basic.ack'{delivery_tag = Tag},
            amqp_channel:cast(ChannelPid, Acknowledge)
    end,
    {noreply, State};
handle_info(#'basic.cancel'{}, State) ->
    %% Rabbit went down. That's nothing to worry about here. Just crash and restart.
    {stop, subscription_canceled, State};
handle_info(connect, undefined) ->
    {ok, RabbitConfigs} = application:get_env(queuemailerl, rabbitmq_configs),
    case connect(RabbitConfigs) of
        {ok, ConnectionPid} ->
            link(ConnectionPid),
            {ok, ChannelPid} = amqp_connection:open_channel(ConnectionPid),
            link(ChannelPid),

            ok = setup_subscription(ChannelPid),

            {noreply, {ConnectionPid, ChannelPid}};
        {error, no_connection_to_mq} ->
            erlang:send_after(?TIMEOUT, self(), connect),
            {noreply, undefined}
    end;
handle_info(Info, State) ->
    %% Some other message to the server pid
    error_logger:info_msg("~p ignoring info ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, undefined) ->
    ok;
terminate(_Reason, {ConnectionPid, ChannelPid}) ->
    catch amqp_channel:close(ChannelPid),
    catch amqp_connection:close(ConnectionPid),
    ok.

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
    {error, no_connection_to_mq}.

setup_subscription(ChannelPid) ->
    %% Queue settings
    {ok, Queue}    = application:get_env(queuemailerl, rabbitmq_queue),
    {ok, Exchange} = application:get_env(queuemailerl, rabbitmq_exchange),

    %% We use the same name for the routing key as the queue name
    RoutingKey = Queue,

    %% Declare the queue
    QueueDeclare = #'queue.declare'{queue = Queue, durable = true},
    #'queue.declare_ok'{} = amqp_channel:call(ChannelPid, QueueDeclare),

    %% Bind the queue to the exchange
    Binding = #'queue.bind'{queue = Queue, exchange = Exchange,
                            routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(ChannelPid, Binding),

    %% Setup the subscription
    Subscription = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = Tag} =
        amqp_channel:subscribe(ChannelPid, Subscription, self()),
    receive
        #'basic.consume_ok'{consumer_tag = Tag} -> ok
    after
        ?TIMEOUT -> {error, timeout}
    end.

