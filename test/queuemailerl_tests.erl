-module(queuemailerl_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

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
        %% Load, configure and start the queuemailerl app
        application:load(queuemailerl),
        application:set_env(queuemailerl, rabbitmq, ?RABBITMQ_CONF),
        queuemailerl:start(),

        %% Setup a connection to rabbitmq to send mail on
        rabbitmq_test_channel(),

        %% Start a dummy SMTP-server that can receive messages
        test_smtp_server:start(?SMTP_PORT)
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
    receive
        {From, To, Data} ->
            ?debugFmt("From: ~p~nTo: ~p~n Data: ~p~n", [From, To, Data])
    after
        1000 ->
            error(timeout)
    end.

rabbitmq_test_channel() ->
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

    {ok, ConnectionPid} = amqp_connection:start(AmqpConnParams),
    {ok, ChannelPid} = amqp_connection:open_channel(ConnectionPid),
    register(test_rabbitmq_send_connection, ConnectionPid),
    register(test_rabbitmq_send_channel, ChannelPid).

