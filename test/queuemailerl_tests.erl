-module(queuemailerl_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(TEST_PROC, test_proc).
-define(SMTP_PORT, 2525).
-define(ERROR_SMTP, [{relay, "localhost"}, {port, 25252}]).
-define(ERROR_FROM, <<"noreply@example.com">>).
-define(RABBITMQ_CONFIGS, [[{username, <<"guest">>},
                            {password, <<"guest">>},
                            {vhost, <<"/">>}]]).
-define(QUEUE, <<"test">>).

%% Macro to silence the error logger. Doesn't seem to work sometimes.
-define(silence(What), error_logger:tty(false),
                       try What after error_logger:tty(true) end).

all_test_() ->
    {setup,
     fun () ->
        %% Load, configure and start the queuemailerl app
        application:load(queuemailerl),
        application:set_env(queuemailerl, rabbitmq_configs, ?RABBITMQ_CONFIGS),
        application:set_env(queuemailerl, rabbitmq_queue, ?QUEUE),
        application:set_env(queuemailerl, retry_count, 10),
        application:set_env(queuemailerl, retry_initial_delay, 1),
        application:set_env(queuemailerl, error_smtp, ?ERROR_SMTP),
        application:set_env(queuemailerl, error_from, ?ERROR_FROM),

        %% Start our application and the dependencies.
        error_logger:tty(false),
        {ok, Apps} = application:ensure_all_started(queuemailerl, permanent),
        error_logger:tty(true),

        %% Setup a rabbitmq environment (queues etc).
        rabbitmq_test_setup(),

        Apps
     end,
     fun (StartedApplications) ->
        %% Delete the test queue
        QueueDelete = #'queue.delete'{queue = ?QUEUE},
        ChannelPid = whereis(test_rabbitmq_send_channel),
        #'queue.delete_ok'{} = amqp_channel:call(ChannelPid, QueueDelete),

        %% Close send connection to RabbitMQ
        amqp_channel:close(whereis(test_rabbitmq_send_channel)),
        amqp_connection:close(whereis(test_rabbitmq_send_connection)),

        %% Stop the application and the dependencies in reverse order
        ?silence(lists:foreach(fun application:stop/1,
                               lists:reverse(StartedApplications)))
     end,
     [fun successful_email/0,
      fun smtp_server_restart/0,
      fun smtp_server_dead/0]
    }.

%% Sends an email successfully.
successful_email() ->

    %% Start the SMTP server
    ?silence(test_smtp_server:start_link(?SMTP_PORT)),

    %% Construct the email event
    From = <<"alice@example.com">>,
    To = <<"bob@example.com">>,
    Event = {[
        {mail, {[
            {from, <<"\"Alice\" <", From/binary, ">">>},
            {to, [<<"\"Bob\" <", To/binary, ">">>]},
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

    %% Wrap it in the RabbitMQ framing and send it
    Publish = #'basic.publish'{
        exchange = <<"amq.direct">>,
        routing_key = ?QUEUE
    },
    Content = #amqp_msg{payload = jiffy:encode(Event)},
    amqp_channel:cast(whereis(test_rabbitmq_send_channel), Publish, Content),

    %% Wait for the worker to send the message and the SMTP server to relay it back to us
    receive
        {From, [To], _Data} -> test_smtp_server:stop()
    after
        10000 -> error(timeout)
    end.

%% Send a mail event over MQ that should be sent. But the SMTP-server is not
%% up and running directly and thus the worker has to wait and try to resend.
smtp_server_restart() ->
    %% Construct the email event
    From = <<"alice@example.com">>,
    To = <<"bob@example.com">>,
    Event = {[
        {mail, {[
            {from, <<"\"Alice\" <", From/binary, ">">>},
            {to, [<<"\"Bob\" <", To/binary, ">">>]},
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

    %% Wrap it in the RabbitMQ framing and send it
    Publish = #'basic.publish'{
        exchange = <<"amq.direct">>,
        routing_key = ?QUEUE
    },
    Content = #amqp_msg{payload = jiffy:encode(Event)},
    amqp_channel:cast(whereis(test_rabbitmq_send_channel), Publish, Content),

    %% Wait to be sure that the worker has been started and has tried to send
    receive
        _ -> ok
    after
        100 -> ok
    end,

    %% Turn of error_logger on tty since this test is suppose to output
    %% error logs
    %error_logger:tty(false),

    %% Start the SMTP server
    ?silence(test_smtp_server:start_link(?SMTP_PORT)),

    %% Wait for the worker to send the message and the SMTP server to relay it back to us
    Result = receive
                 {From, [To], _Data} -> true
             after
                 1000 -> false
             end,
    test_smtp_server:stop(),
    error_logger:tty(true),
    ?assert(Result).

%% Sending an mail to a dead SMTP server should result in a email being sent to the sender
%% after the retry limit has been reached. This email should contain the original as an
%% attachment.
smtp_server_dead() ->
    %% Turn of error_logger on tty since this test is suppose to output
    %% error logs
    %error_logger:tty(false),

    %% Start the ERROR SMTP server
    ErrorSmtpPort = proplists:get_value(port, ?ERROR_SMTP),
    ?silence(test_smtp_server:start_link(ErrorSmtpPort)),
    %% Construct the email event
    From = <<"alice@example.com">>,
    To = <<"bob@example.com">>,
    Event = {[
        {mail, {[
            {from, <<"\"Alice\" <", From/binary, ">">>},
            {to, [<<"\"Bob\" <", To/binary, ">">>]},
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

    %% Wrap it in the RabbitMQ framing and send it
    Publish = #'basic.publish'{
        exchange = <<"amq.direct">>,
        routing_key = ?QUEUE
    },
    Content = #amqp_msg{payload = jiffy:encode(Event)},
    amqp_channel:cast(whereis(test_rabbitmq_send_channel), Publish, Content),

    %% Wait for the error message to arrive
    Result = receive
                 {_From, [_To], _Data} -> true
             after
                 5000 -> false
             end,
    test_smtp_server:stop(),
    error_logger:tty(true),
    ?assert(Result).

%% @doc Make a test connection to the RabbitMQ broker and start a temporary
%% queue that will be used to publish mails to.
rabbitmq_test_setup() ->
    [RabbitProps] = ?RABBITMQ_CONFIGS,
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
    ok.
