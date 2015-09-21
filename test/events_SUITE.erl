-module(events_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([simple_mail_1/1]).
-export([simple_mail_2/1]).
-export([utf8_mail_1/1]).
-export([utf8_mail_2/1]).
-export([parts_mail_1/1]).
-export([parts_mail_2/1]).
-export([error_mail_1/1]).

all() -> [
          simple_mail_1,
          simple_mail_2,
          utf8_mail_1,
          utf8_mail_2,
          parts_mail_1,
          parts_mail_2,
          error_mail_1
         ].

simple_mail_1(_Config) ->
    TestMail = <<"{"
                 "  \"mail\": {"
                 "    \"from\": \"A B <a.b@c.se>\","
                 "    \"to\": [\"z.u@d.se\", \"Y V <y.v@d.se>\"], \"cc\": [\"<q.r@d.se>\"],"
                 "    \"extra-headers\": { },"
                 "    \"body\": [ ]"
                 "  },"
                 "  \"smtp\": {"
                 "    \"relay\": \"smtp.nowhere.test\", \"port\": 25,"
                 "    \"username\": \"nobody\", \"password\": \"secret\""
                 "  },"
                 "  \"error\": {"
                 "    \"to\": \"<admin@c.se>\","
                 "    \"subject\": \"This is a subject\","
                 "    \"body\": \"This is a message\""
                 "  }"
                 "}">>,
    {ok, {event,
          {From, To, Mail},
          {smtp, SMTPRelay, _Port, _Username, _Password},
          {error, ErrorTo, ErrorSubject, ErrorMsg}}} = queuemailerl_event:parse(TestMail),
    ?assertEqual(From, <<"<a.b@c.se>">>),
    ?assertEqual(lists:sort(To), lists:sort([<<"<z.u@d.se>">>,
                                             <<"<y.v@d.se>">>,
                                             <<"<q.r@d.se>">>])),
    ?assertEqual(SMTPRelay, <<"smtp.nowhere.test">>),
    ?assertEqual(ErrorTo, <<"<admin@c.se>">>),
    ?assertEqual(ErrorSubject, <<"This is a subject">>),
    ?assertEqual(ErrorMsg, <<"This is a message">>),

    Mail2 = clean_up_mail(Mail),

    ExpectedMail = <<"From: A B <a.b@c.se>\r\n"
                     "To: z.u@d.se, Y V <y.v@d.se>\r\n"
                     "Cc: q.r@d.se\r\n"
                     "Date: _\r\n"
                     "Content-Type: multipart/mixed;\r\n"
                     "\tboundary=\"_1_\"\r\n"
                     "MIME-Version: 1.0\r\n"
                     "Message-ID: _\r\n"
                     "\r\n"
                     "\r\n"
                     "--_1_--\r\n">>,
    ?assertEqual(isolate_difference(Mail2, ExpectedMail), {<<>>, <<>>}),
    ok.

simple_mail_2(_Config) ->
    TestMail = <<"{"
                 "  \"mail\": {"
                 "    \"from\": \"A B <a.b@c.se>\","
                 "    \"to\": [\"z.u@d.se\", \"Y V <y.v@d.se>\"], \"cc\": [\"<q.r@d.se>\"],"
                 "    \"subject\": \"Subject\","
                 "    \"extra-headers\": { },"
                 "    \"body\": \"Not empty\""
                 "  },"
                 "  \"smtp\": {"
                 "    \"relay\": \"smtp.nowhere.test\", \"port\": 25,"
                 "    \"username\": \"nobody\", \"password\": \"secret\""
                 "  },"
                 "  \"error\": {"
                 "    \"to\": \"<admin@c.se>\","
                 "    \"subject\": \"This is a subject\","
                 "    \"body\": \"This is a message\""
                 "  }"
                 "}">>,
    {ok, {event, {_From, _To, Mail}, _SMTP, _ErrorInfo}} = queuemailerl_event:parse(TestMail),

    Mail1 = clean_up_mail(Mail),

    ExpectedMail = <<"From: A B <a.b@c.se>\r\n"
                     "To: z.u@d.se, Y V <y.v@d.se>\r\n"
                     "Cc: q.r@d.se\r\n"
                     "Subject: Subject\r\n"
                     "Date: _\r\n"
                     "MIME-Version: 1.0\r\n"
                     "Message-ID: _\r\n"
                     "\r\n"
                     "Not empty">>,
    ?assertEqual(isolate_difference(Mail1, ExpectedMail), {<<>>, <<>>}),
    ok.

%% @doc This tests short mails with embeded UTF-8 characters, these will be base64 encoded.
%% @end
utf8_mail_1(_Config) ->
    TestMail = unicode:characters_to_binary(
                 "{"
                 "  \"mail\": {"
                 "    \"from\": \"A B <a.b@c.se>\","
                 "    \"to\": [\"z.u@d.se\", \"Y V <y.v@d.se>\"], \"cc\": [\"<q.r@d.se>\"],"
                 "    \"extra-headers\": { },"
                 "    \"body\": \"Räksmörgås\""
                 "  },"
                 "  \"smtp\": {"
                 "    \"relay\": \"smtp.nowhere.test\", \"port\": 25,"
                 "    \"username\": \"nobody\", \"password\": \"secret\""
                 "  },"
                 "  \"error\": {"
                 "    \"to\": \"<admin@c.se>\","
                 "    \"subject\": \"This is a subject\","
                 "    \"body\": \"This is a message\""
                 "  }"
                 "}", utf8),
    Base64Body = list_to_binary(base64:encode_to_string(<<"Räksmörgås"/utf8>>)),
    {ok, {event, {_From, _To, Mail}, _SMTP, _ERROR}} = queuemailerl_event:parse(TestMail),

    Mail2 = clean_up_mail(Mail),

    ExpectedMail = <<"From: A B <a.b@c.se>\r\n"
                     "To: z.u@d.se, Y V <y.v@d.se>\r\n"
                     "Cc: q.r@d.se\r\n"
                     "Date: _\r\n"
                     "Content-Type: text/plain;\r\n"
                     "\tcharset=utf-8\r\n"
                     "Content-Transfer-Encoding: base64\r\n"
                     "MIME-Version: 1.0\r\n"
                     "Message-ID: _\r\n"
                     "\r\n",
                     Base64Body/binary, "\r\n">>,
    {Diff1, Diff2} = isolate_difference(Mail2, ExpectedMail),
    ?assertEqual(Diff1, Diff2),
    ok.

%% @doc This tests long mails with embeded UTF-8 characters, these will be "quoted-printable"
%% encoded.
%% @end
utf8_mail_2(_Config) ->
    %% The body strings in the following mail is made long so that the library used to make e-mail
    %% formatted strings doesn't base64 encode them. (They will be transfer encoded with
    %% "quoted-printable" instead.
    TestMail = <<"{"
                 "  \"mail\": {"
                 "    \"from\": \"A B <a.b@c.se>\","
                 "    \"to\": [\"z.u@d.se\", \"Y V <y.v@d.se>\"], \"cc\": [\"<q.r@d.se>\"],"
                 "    \"extra-headers\": { \"Content-Type\": \"multipart/alternative\" },"
                 "    \"subject\": \"Here we have a rather long subject containing åÅö\","
                 "    \"body\": ["
                 "      {"
                 "        \"headers\": {"
                 "          \"content-type\": \"text/plain\""
                 "        },"
                 "        \"body\": \"This text is not to be base64 encoded, it is long enough not"
                 " to, Räksmörgås\""
                 "      },"
                 "      {"
                 "        \"headers\": {"
                 "          \"content-type\": \"text/html\""
                 "        },"
                 "        \"body\": \"<html><head><title></title></head><body>"
                 "<b>Räksmörgås</b>"
                 "</body></html>\""
                 "      }"
                 "    ]"
                 "  },"
                 "  \"smtp\": {"
                 "    \"relay\": \"smtp.nowhere.test\", \"port\": 25,"
                 "    \"username\": \"nobody\", \"password\": \"secret\""
                 "  },"
                 "  \"error\": {"
                 "    \"to\": \"<admin@c.se>\","
                 "    \"subject\": \"This is a subject\","
                 "    \"body\": \"This is a message\""
                 "  }"
                 "}"/utf8>>,
    {ok, {event, {_From, _To, Mail}, _SMTP, _ERROR}} = queuemailerl_event:parse(TestMail),

    Mail2 = clean_up_mail(Mail),

    ExpectedMail = <<"From: A B <a.b@c.se>\r\n"
                     "To: z.u@d.se, Y V <y.v@d.se>\r\n"
                     "Cc: q.r@d.se\r\n"
                     "Subject: =?UTF-8?Q?Here=20we=20have=20a=20rather=20"
                     "long=20subject=20contai?=\r\n =?UTF-8?Q?ning=20=C3=A5=C3=85=C3=B6?=\r\n"
                     "Date: _\r\n"
                     "Content-Type: multipart/alternative;\r\n"
                     "\tboundary=\"_1_\"\r\n"
                     "MIME-Version: 1.0\r\n"
                     "Message-ID: _\r\n"
                     "\r\n"
                     "\r\n"
                     "--_1_\r\n"
                     "Content-Type: text/plain;\r\n"
                     "\tcharset=utf-8\r\n"
                     "Content-Disposition: inline\r\n"
                     "Content-Transfer-Encoding: quoted-printable\r\n"
                     "\r\n"
                     "This text is not to be base64 encoded, it is long enough not to, =\r\n"
                     "R=C3=A4ksm=C3=B6rg=C3=A5s\r\n"
                     "--_1_\r\n"
                     "Content-Type: text/html;\r\n"
                     "\tcharset=utf-8\r\n"
                     "Content-Disposition: inline\r\n"
                     "Content-Transfer-Encoding: quoted-printable\r\n"
                     "\r\n"
                     "<html><head><title></title></head><body><b>R=C3=A4ksm=C3=B6rg=C3=A5s</b>"
                     "</b=\r\nody></html>\r\n"
                     "--_1_--\r\n"/utf8>>,
    {Diff1, Diff2} = isolate_difference(Mail2, ExpectedMail),
    ?assertEqual(Diff1, Diff2),
    ok.

parts_mail_1(_Config) ->
    TestMail = <<"{"
                 "  \"mail\": {"
                 "    \"from\": \"A B <a.b@c.se>\", \"to\": [\"z.u@d.se\"],"
                 "    \"extra-headers\": { },"
                 "    \"body\": ["
                 "      {"
                 "        \"headers\": { \"content-type\": \"multipart/alternative\" },"
                 "        \"body\": ["
                 "          {"
                 "            \"headers\": { \"content-type\": \"text/plain\" },"
                 "            \"body\": \"Plain text\""
                 "          },"
                 "          {"
                 "            \"headers\": { \"content-type\": \"text/html\" },"
                 "            \"body\": \"<html></html>\""
                 "          }"
                 "        ]"
                 "      },"
                 "      {"
                 "        \"headers\": {"
                 "          \"content-type\": \"image/png\","
                 "          \"content-filename\": \"test.png\""
                 "        },"
                 "        \"body\": \"ABCDEF==\""
                 "      }"
                 "    ]"
                 "  },"
                 "  \"smtp\": {"
                 "    \"relay\": \"smtp.nowhere.test\", \"port\": 25,"
                 "    \"username\": \"nobody\",\"password\": \"secret\""
                 "  },"
                 "  \"error\": {"
                 "    \"to\": \"<admin@c.se>\","
                 "    \"subject\": \"This is a subject\","
                 "    \"body\": \"This is a message\""
                 "  }"
                 "}">>,
    {ok, {event, {_From, _To, Mail}, _SMTP, _ErrorInfo}} = queuemailerl_event:parse(TestMail),

    Mail3 = clean_up_mail(Mail),

    ExpectedMail = <<"From: A B <a.b@c.se>\r\n" "To: z.u@d.se\r\n" "Date: _\r\n"
                     "Content-Type: multipart/mixed;\r\n" "\tboundary=\"_1_\"\r\n"
                     "MIME-Version: 1.0\r\n" "Message-ID: _\r\n" "\r\n" "\r\n"
                     "--_1_\r\n" "Content-Type: multipart/alternative;\r\n"
                     "\tboundary=\"_2_\"\r\n" "Content-Disposition: inline\r\n"
                     "\r\n" "\r\n" "--_2_\r\n"
                     "Content-Type: text/plain;\r\n" "\tcharset=utf-8\r\n"
                     "Content-Disposition: inline\r\n"
                     "\r\n" "Plain text\r\n" "--_2_\r\n"
                     "Content-Type: text/html;\r\n" "\tcharset=us-ascii\r\n"
                     "Content-Disposition: inline\r\n"
                     "\r\n" "<html></html>\r\n" "--_2_--\r\n" "\r\n" "--_1_\r\n"
                     "Content-Type: image/png\r\n"
                     "Content-Disposition: attachment;\r\n" "\t filename=\"test.png\"\r\n" "\r\n"
                     "ABCDEF==\r\n" "--_1_--\r\n">>,
    {Diff1, Diff2} = isolate_difference(ExpectedMail, Mail3),
    ?assertEqual(Diff1, Diff2),
    ?assertEqual(Diff1, <<>>),
    ?assertEqual(Diff2, <<>>),
    ok.

parts_mail_2(_Config) ->
    TestMail = <<"{"
                 "  \"mail\": {"
                 "    \"from\": \"A B <a.b@c.se>\", \"to\": [\"z.u@d.se\"],"
                 "    \"extra-headers\": { },"
                 "    \"body\": ["
                 "      {"
                 "        \"headers\": { \"content-type\": \"multipart/alternative\" },"
                 "        \"body\": ["
                 "          {"
                 "            \"headers\": {"
                 "              \"content-type\": \"text/plain; charset=utf-8\""
                 "          },"
                 "            \"body\": \"Plain text\""
                 "          },"
                 "          {"
                 "            \"headers\": { \"content-type\": \"text/html\" },"
                 "            \"body\": \"<html></html>\""
                 "          }"
                 "        ]"
                 "      }"
                 "    ]"
                 "  },"
                 "  \"smtp\": {"
                 "    \"relay\": \"smtp.nowhere.test\", \"port\": 25,"
                 "    \"username\": \"nobody\",\"password\": \"secret\""
                 "  },"
                 "  \"error\": {"
                 "    \"to\": \"<admin@c.se>\","
                 "    \"subject\": \"This is a subject\","
                 "    \"body\": \"This is a message\""
                 "  }"
                 "}">>,
    {ok, {event, {_From, _To, Mail}, _SMTP, _ErrorInfo}} = queuemailerl_event:parse(TestMail),

    Mail3 = clean_up_mail(Mail),

    ExpectedMail = <<"From: A B <a.b@c.se>\r\n" "To: z.u@d.se\r\n" "Date: _\r\n"
                     "Content-Type: multipart/alternative;\r\n" "\tboundary=\"_1_\"\r\n"
                     "MIME-Version: 1.0\r\n" "Message-ID: _\r\n" "\r\n" "\r\n"
                     "--_1_\r\n"
                     "Content-Type: text/plain;\r\n" "\tcharset=utf-8\r\n"
                     "Content-Disposition: inline\r\n"
                     "\r\n" "Plain text\r\n" "--_1_\r\n"
                     "Content-Type: text/html;\r\n" "\tcharset=us-ascii\r\n"
                     "Content-Disposition: inline\r\n"
                     "\r\n" "<html></html>\r\n" "--_1_--\r\n">>,
    {Diff1, Diff2} = isolate_difference(ExpectedMail, Mail3),
    ?assertEqual(Diff1, Diff2),
    ?assertEqual(Diff1, <<>>),
    ?assertEqual(Diff2, <<>>),
    ok.

error_mail_1(_Config) ->
    application:set_env(queuemailerl, error_from, <<"noreply@example.com">>),

    TestMail = <<"{"
                 "  \"mail\": {"
                 "    \"from\": \"A B <a.b@c.se>\","
                 "    \"to\": [\"z.u@d.se\"],"
                 "    \"extra-headers\": { },"
                 "    \"body\": \"Not empty\""
                 "  },"
                 "  \"smtp\": {"
                 "    \"relay\": \"smtp.nowhere.test\", \"port\": 25,"
                 "    \"username\": \"nobody\", \"password\": \"secret\""
                 "  },"
                 "  \"error\": {"
                 "    \"to\": \"<admin@c.se>\","
                 "    \"subject\": \"This is a subject\","
                 "    \"body\": \"This is a message\""
                 "  }"
                 "}">>,
    {ok, Event} = queuemailerl_event:parse(TestMail),
    {From, [To], Mail0} = queuemailerl_event:build_error_mail(Event),
    {_From, _To, OrigMail} = queuemailerl_event:get_mail(Event),
    ?assertEqual(From, <<"<noreply@example.com>">>),
    ?assertEqual(To, <<"<admin@c.se>">>),
    Mail1 = clean_up_mail(Mail0),
    ExpectedMail = <<"From: noreply@example.com\r\n"
                   "To: admin@c.se\r\n"
                   "Subject: This is a subject\r\n"
                   "Content-Type: multipart/mixed;\r\n"
                   "\tboundary=\"_1_\"\r\n"
                   "MIME-Version: 1.0\r\n"
                   "Date: _\r\n"
                   "Message-ID: _\r\n"
                   "\r\n"
                   "\r\n"
                   "--_1_\r\n"
                   "Content-Disposition: inline\r\n"
                   "\r\n"
                   "This is a message\r\n"
                   "--_1_\r\n"
                   "Content-Type: message/rfc822;\r\n"
                   "\tname=Mail\r\n"
                   "Content-Disposition: inline;\r\n"
                   "\tfilename=Mail.eml\r\n"
                   "\r\n">>,
    {Diff1, Diff2} = isolate_difference(ExpectedMail, Mail1),
    ?assertEqual(Diff1, <<>>),
    {Diff3, Diff4} = isolate_difference(OrigMail, Diff2),
    ?assertEqual(Diff3, <<>>),
    ?assertEqual(Diff4, <<"\r\n--_1_--\r\n">>),
    ?assertNotEqual(Diff2, <<>>),
    ok.

%% Removes common parts of A and B. Returns them trimmed by identical prefix
%% and suffix. This makes it easier to locate the difference between two large
%% binaries.
isolate_difference(A, B) ->
    PrefixLength = binary:longest_common_prefix([A, B]),
    <<_:PrefixLength/binary, A1/binary>> = A,
    <<_:PrefixLength/binary, B1/binary>> = B,
    {A1, B1}.

remove_boundaries([], _Count, Mail) ->
    Mail;
remove_boundaries([Boundary | Boundaries], Count, Mail0) ->
    Placeholder = <<"_", (list_to_binary(integer_to_list(Count)))/binary, "_">>,
    Mail1 = binary:replace(Mail0, Boundary, Placeholder, [global]),
    remove_boundaries(Boundaries, Count + 1, Mail1).

clean_up_mail(Mail0) ->
    Mail1 = case re:run(Mail0, <<"(?<=\r\n\tboundary=\")[^\r]+(?=\"\r\n)">>,
                        [{capture, all, binary}, global]) of
                {match, Boundaries} ->
                    remove_boundaries(Boundaries, 1, Mail0);
                nomatch -> Mail0
            end,
    Mail2 = re:replace(Mail1, <<"(?<=\r\nDate: )[^\r]+(?=\r\n)">>, <<"_">>,
                       [{return, binary}]),
    Mail3 = re:replace(Mail2, <<"(?<=\r\nMessage-ID: )[^\r]+(?=\r\n)">>, <<"_">>,
                       [{return, binary}]),
    Mail3.

