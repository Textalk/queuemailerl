-module(queuemailerl_event_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_EVENT_DATA, {[
        {mail, {[
            {from, <<"Alice <alice@example.com>">>},
            {to, [<<"Bob <bob@example.com>">>]},
            {'extra-headers', {[
                {<<"Subject">>, <<"Test">>},
                {<<"Date">>, <<"Thu, 27 Nov 2014 19:33:09 +0100">>}
            ]}},
            {body, <<"TestBody">>}
        ]}},
        {smtp, {[
            {relay, <<"dummyrelay">>},
            {port, 2500},
            {username, alice},
            {password, <<"d9Jeaoid9%ud4">>}
        ]}},
        {error, {[
            {to, <<"email-administrator@example.com">>},
            {subject, <<"Subject in error report mail">>},
            {body, <<"The message to be sent in the event of error">>}
        ]}}
    ]}).

-define(TEST_EVENT_PARTS_DATA, {[
        {mail, {[
            {from, <<"Alice <alice@example.com>">>},
            {to, [<<"bob@example.com">>]},
            {'extra-headers', {[
                {<<"Subject">>, <<"Test">>},
                {<<"Date">>, <<"Thu, 27 Nov 2014 19:33:09 +0100">>}
            ]}},
            {parts, [
                {[{'headers', {[{'content-type', <<"text/plain">>},
                                {'content-disposition', <<"inline">>}]}},
                  {'body', <<"Hello!">>}]},
                {[{'headers', {[{'content-type', <<"application/json">>},
                                {'content-disposition', <<"attachment; filename=apa.txt">>}]}},
                  {'body', <<"{\"foo\": 42}">>}]}
            ]}
        ]}},
        {smtp, {[
            {relay, <<"dummyrelay">>},
            {port, 2500},
            {username, alice},
            {password, <<"d9Jeaoid9%ud4">>}
        ]}},
        {error, {[
            {to, <<"email-administrator@example.com">>},
            {subject, <<"Subject in error report mail">>},
            {body, <<"The message to be sent in the event of error">>}
        ]}}
    ]}).

-define(TEST_EVENT_BAD1_DATA, {[
        {mail, {[
            {from, <<"Alice <alice@example.com>">>},
            {to, [<<"bob@example.com">>]},
            {'extra-headers', {[
                {<<"Subject">>, <<"Test">>},
                {<<"Date">>, <<"Thu, 27 Nov 2014 19:33:09 +0100">>}
            ]}}
        ]}},
        {smtp, {[
            {relay, <<"dummyrelay">>},
            {port, 2500},
            {username, alice},
            {password, <<"d9Jeaoid9%ud4">>}
        ]}},
        {error, {[
            {to, <<"email-administrator@example.com">>},
            {subject, <<"Subject in error report mail">>},
            {body, <<"The message to be sent in the event of error">>}
        ]}}
    ]}).

-define(TEST_EVENT_BAD2_DATA, {[
        {mail, {[
            {from, <<"Alice <alice@example.com>">>},
            {to, [<<"bob@example.com">>]},
            {'extra-headers', {[
                {<<"Subject">>, <<"Test">>},
                {<<"Date">>, <<"Thu, 27 Nov 2014 19:33:09 +0100">>}
            ]}},
            {parts, [
                {[{'headers', {[{'content-type', <<"text/plain">>}]}},
                  {'body', <<"Hello!">>}]}
            ]},
            {body, <<"Hello">>}
        ]}},
        {smtp, {[
            {relay, <<"dummyrelay">>},
            {port, 2500},
            {username, alice},
            {password, <<"d9Jeaoid9%ud4">>}
        ]}},
        {error, {[
            {to, <<"email-administrator@example.com">>},
            {subject, <<"Subject in error report mail">>},
            {body, <<"The message to be sent in the event of error">>}
        ]}}
    ]}).

get_bad_mail_test() ->
    ?assertMatch({error, _, _},
                 queuemailerl_event:parse(jiffy:encode(?TEST_EVENT_BAD1_DATA))),
    ?assertMatch({error, _, _},
                 queuemailerl_event:parse(jiffy:encode(?TEST_EVENT_BAD2_DATA))),
    ok.

get_mail_test() ->
    {ok, Event} = queuemailerl_event:parse(jiffy:encode(?TEST_EVENT_DATA)),
    {From, To, Mail} = queuemailerl_event:get_mail(Event),

    %% Check from and to
    ?assertEqual(<<"<alice@example.com>">>, From),
    ?assertEqual([<<"<bob@example.com>">>], To),

    %% Check the mail itself
    OrigMail =
        <<"From: Alice <alice@example.com>\r\n"
          "To: Bob <bob@example.com>\r\n"
          "Subject: Test\r\n"
          "Date: Thu, 27 Nov 2014 19:33:09 +0100\r\n"
          "MIME-Version: 1.0\r\n"
          "\r\n"
          "TestBody">>,

    %% Remove the Message-ID row since it isn't static
    {match, [{Start, Length}]} = re:run(Mail, <<"Message-ID: <.*@.*>\r\n">>),
    MailBeginning = erlang:binary_part(Mail, 0, Start),
    {_MessageID, MailRest} = erlang:split_binary(Mail, Start+Length),
    Mail2 = <<MailBeginning/binary, MailRest/binary>>,

    ?assertEqual(OrigMail, Mail2).

get_parts_mail_test() ->
    Data = ?TEST_EVENT_PARTS_DATA,
    {ok, Event} = queuemailerl_event:parse(jiffy:encode(Data)),
    {_From, _To, Mail} = queuemailerl_event:get_mail(Event),
    {match, [Boundary]} = re:run(Mail, <<"boundary=\"([^\"]*)\"">>,
                                 [{capture, all_but_first, binary}]),
    {match, [MessageID]} = re:run(Mail, <<"Message-ID: (<[^>]+>)\r\n">>,
                                  [{capture, all_but_first, binary}]),
    ExpectedMail =
        <<"From: Alice <alice@example.com>\r\n"
          "To: bob@example.com\r\n"
          "Subject: Test\r\n"
          "Date: Thu, 27 Nov 2014 19:33:09 +0100\r\n"
          "Content-Type: multipart/mixed;\r\n"
          "\tboundary=\"", Boundary/binary, "\"\r\n"
          "MIME-Version: 1.0\r\n"
          "Message-ID: ", MessageID/binary, "\r\n"
          "\r\n"
          "\r\n"
          "--", Boundary/binary, "\r\n"
          "Content-Disposition: inline\r\n"
          "\r\n"
          "Hello!\r\n"
          "--", Boundary/binary, "\r\n"
          "Content-Type: application/json\r\n"
          "Content-Disposition: attachment;\r\n"
          "\t filename=apa.txt\r\n"
          "\r\n",
          "{\"foo\": 42}\r\n",
          "--", Boundary/binary, "--\r\n">>,

    {ExpectedMail1, Mail1} = isolate_difference(ExpectedMail, Mail),
    ?assertEqual(ExpectedMail1, Mail1),
    ok.



build_error_mail_test() ->
    application:set_env(queuemailerl, error_from, <<"noreply@example.com">>),

    %% Build the error mail from the event data.
    {ok, Event} = queuemailerl_event:parse(jiffy:encode(?TEST_EVENT_DATA)),
    {From, To, Mail} = queuemailerl_event:build_error_mail(Event),

    %% Original is built from the Data, but that's another test case.
    {_, _, OrigMail} = queuemailerl_event:get_mail(Event),

    %% Check from and to
    ?assertEqual(<<"<noreply@example.com>">>, From),
    ?assertEqual([<<"<email-administrator@example.com>">>], To),

    %% Extract things that vary, e.g. time and random stuff.
    {match, [Boundary]} = re:run(Mail, <<"boundary=\"([^\"]*)\"">>,
                                 [{capture, all_but_first, binary}]),
    {match, [Date]} = re:run(Mail, <<"Date: ([^\\r\\n]*)\r\n">>,
                             [{capture, all_but_first, binary}]),
    {match, [MessageID]} = re:run(Mail, <<"Message-ID: (<[^>]+>)\r\n">>,
                                  [{capture, all_but_first, binary}]),
    %% TODO: Add the failing SMTP settings (excluding the password).
    ExpectedMail =
        <<"From: noreply@example.com\r\n"
          "To: email-administrator@example.com\r\n"
          "Subject: Subject in error report mail\r\n"
          "Content-Type: multipart/mixed;\r\n"
          "\tboundary=\"", Boundary/binary, "\"\r\n"
          "MIME-Version: 1.0\r\n"
          "Date: ", Date/binary, "\r\n"
          "Message-ID: ", MessageID/binary, "\r\n"
          "\r\n"
          "\r\n"
          "--", Boundary/binary, "\r\n"
          "Content-Disposition: inline\r\n"
          "\r\n"
          "The message to be sent in the event of error\r\n"
          "--", Boundary/binary, "\r\n"
          "Content-Type: message/rfc822;\r\n"
          "\tname=Mail\r\n"
          "Content-Disposition: inline;\r\n"
          "\tfilename=Mail.eml\r\n"
          "\r\n",
          OrigMail/binary, "\r\n"
          "--", Boundary/binary, "--\r\n">>,
    %% Isolate the differences before the assertion so that if the assertment
    %% fails, it's easier to locate where the actual difference is.
    {Expected1, Mail1} = isolate_difference(ExpectedMail, Mail),
    ?assertEqual(Expected1, Mail1),
    ok.

%% Removes common parts of A and B. Returns them trimmed by identical prefix
%% and suffix. This makes it easier to locate the difference between two large
%% binaries.
isolate_difference(A, B) ->
    PrefixLength = binary:longest_common_prefix([A, B]),
    <<_:PrefixLength/binary, A1/binary>> = A,
    <<_:PrefixLength/binary, B1/binary>> = B,
    {A1, B1}.
