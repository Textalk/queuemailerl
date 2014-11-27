-module(queuemailerl_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(TEST_PROC, test_proc).
-define(SMTP_PORT, 2525).
-define(RABBITMQ_CONF,
    [
     {username, <<"test">>},
     {password, <<"test">>},
     {vhost, <<"/test">>},
     {queue, <<"test">>}
    ]
).

all_test_() ->
    {setup,
     fun () ->
        %% Setup a rabbitmq environment (queues etc).
        RabbitMQConf = rabbitmq_test_setup(),

        %% Load, configure and start the queuemailerl app
        application:load(queuemailerl),
        application:set_env(queuemailerl, rabbitmq, RabbitMQConf),
        queuemailerl:start(),

        %% Start a dummy SMTP-server that can receive messages
        test_smtp_server:start(?SMTP_PORT, ?TEST_PROC),
        RabbitMQConf
     end,
     fun (_) ->
        %% Close send connection to RabbitMQ
        amqp_channel:close(whereis(test_rabbitmq_send_channel)),
        amqp_connection:close(whereis(test_rabbitmq_send_connection)),

        %% Close the test smtp server
        test_smtp_server:stop(),

        %% Close the application
        queuemailerl:stop()
     end,
     [fun successful_email/0]
    }.

%% Sends an email successfully.
successful_email() ->
    %% Register ourselves to get the mail from the server
    register(?TEST_PROC, self()),

    Event = {[
        {mail, {[
            {from, <<"\"Alice\" <alice@example.com>">>},
            {to, [<<"\"Bob\" <bob@example.com>">>]},
            {'extra-headers', {[
                {<<"Subject">>, <<"Test">>}
            ]}},
            {body, <<"TestBody">>}
        ]}},
        {smtp, {[
            {relay, localhost},
            {port, ?SMTP_PORT},
            {username, alice},
            {password, <<"d9Jeaoid9%ud4">>}
        ]}},
        {error, {[
            {to, <<"email-administrator@example.com">>},
            {subject, <<"Subject in error report mail">>},
            {body, <<"The message to be sent in the event of error">>}
        ]}}
    ]},
    Publish = #'basic.publish'{
        exchange = <<"amq.direct">>,
        routing_key = proplists:get_value(queue, ?RABBITMQ_CONF)
    },
    Content = #amqp_msg{payload = jiffy:encode(Event)},
    amqp_channel:cast(whereis(test_rabbitmq_send_channel), Publish, Content),
    ?debugFmt("Sent mail to queue from ~p.", [self()]),
    receive
        Msg ->
            ?debugFmt("Msg: ~p", [Msg])
    after
        1000 ->
            error(timeout)
    end.

%% @doc Make a test connection to the RabbitMQ broker and start a temporary
%% queue that will be used to publish mails to.
rabbitmq_test_setup() ->
    RabbitProps = ?RABBITMQ_CONF,
    AmqpConnParams = #amqp_params_network{
        username           = proplists:get_value(username, RabbitProps),
        password           = proplists:get_value(password, RabbitProps),
        virtual_host       = proplists:get_value(vhost, RabbitProps, <<"/">>),
        host               = proplists:get_value(host, RabbitProps, "localhost"),
        port               = proplists:get_value(port, RabbitProps, 5672),
        heartbeat          = 5,
        connection_timeout = 60000
    },

    %% Create the test connection and channel
    {ok, ConnectionPid} = amqp_connection:start(AmqpConnParams),
    {ok, ChannelPid} = amqp_connection:open_channel(ConnectionPid),
    register(test_rabbitmq_send_connection, ConnectionPid),
    register(test_rabbitmq_send_channel, ChannelPid),

    %% Delete the old test queue
    QueueDelete = #'queue.delete'{queue = proplists:get_value(queue, RabbitProps)},
    #'queue.delete_ok'{} = amqp_channel:call(ChannelPid, QueueDelete),

    RabbitProps.
