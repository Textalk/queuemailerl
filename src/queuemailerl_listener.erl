%% @doc Receives messages from RabbitMQ and dispatches mail sending processes.
-module(queuemailerl_listener).

-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Default queue name
-define(DEFAULT_QUEUE, <<"queuemailerl">>).

%% The exchange that the queue is bound to
-define(EXCHANGE, <<"amq.direct">>).

%% Maximum time to wait when subscribing
-define(SUBSCRIBE_TIMEOUT, 10000).

-include_lib("amqp_client/include/amqp_client.hrl").

%% --- Public API ---

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% +--- Gen_server callbacks -----------------------------+
%% |                                                      |
%% |  The state of the gen_server is a channel pid only.  |
%% |                                                      |
%% +------------------------------------------------------+

init([]) ->
    %% Queue settings
    {ok, RabbitProps} = application:get_env(queuemailerl, rabbitmq),
    Queue = proplists:get_value(queue, RabbitProps, ?DEFAULT_QUEUE),

    %% We use the same name for the routing key as the queue name
    RoutingKey = Queue,

    %% Get the channel
    ChannelPid = queuemailerl_amqp_mgr:get_channel(),

    %% Declare the queue
    QueueDeclare = #'queue.declare'{queue = Queue, durable = true},
    #'queue.declare_ok'{} = amqp_channel:call(ChannelPid, QueueDeclare),

    %% Bind the queue to the exchange
    Binding = #'queue.bind'{queue = Queue, exchange = ?EXCHANGE,
                            routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(ChannelPid, Binding),

    %% Setup the subscription
    Subscription = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = Tag} =
        amqp_channel:subscribe(ChannelPid, Subscription, self()),
    receive
        #'basic.consume_ok'{consumer_tag = Tag} ->
            {ok, ChannelPid} %% The state is a channel pid only
    after
        ?SUBSCRIBE_TIMEOUT -> {error, timeout}
    end.

%% @doc There are no recognized gen_server calls.
handle_call(_Call, _From, _State) ->
    error(badarg).

%% @doc When a worker is done, it should cast an {ack, Tag} back to us.
handle_cast({ack, Tag}, Channel) ->
    Acknowledge = #'basic.ack'{delivery_tag = Tag},
    amqp_channel:cast(Channel, Acknowledge),
    {noreply, Channel}.

%% @doc Handles incoming messages from RabbitMQ.
handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, Channel) ->
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
            amqp_channel:cast(Channel, Acknowledge)
    end,
    {noreply, Channel};
handle_info(Info, State) ->
    %% Some other message to the server pid
    error_logger:info_msg("~p ignoring info ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
