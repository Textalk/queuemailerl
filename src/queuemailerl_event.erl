%% @doc Abstract datatype for an event.
-module(queuemailerl_event).

-export([parse/1, build_mail/1, get_smtp_options/1, build_error_mail/2]).
-export_type([event/0]).

-record(mail, {from, to, cc, bcc, headers, body}).
-record(smtp, {relay, port, username, password}).
-record(error, {to, subject, body}).
-record(event, {mail, smtp, error}).

-opaque event() :: #event{}.

%% @doc Parses a raw JSON event and raises an error if the event is incorrect.
-spec parse(binary()) -> event().
parse(RawEvent) ->
    {Props} = jiffy:decode(RawEvent),
    #event{mail = parse_mail_part(proplists:get_value(<<"mail">>, Props)),
           smtp = parse_smtp_part(proplists:get_value(<<"smtp">>, Props)),
           error = parse_error_part(proplists:get_value(<<"error">>, Props))}.

%% @doc Returns a triple that can as the first argument to
%% gen_smtp_client:send/2,1 and gen_smtp_client:send_blocking/2.
%%
%% TODO: Convert all headers to header-case using to_header_case/1.
%% TODO: Add a Date header if not preset.
-spec build_mail(event()) ->
    {MailFrom :: binary(), RcptTo :: [binary()], Email :: iodata()}.
build_mail(#event{mail = #mail{from = From, to = To, cc = Cc, bcc = Bcc,
                               headers = Headers, body = Body}}) ->
    Headers1 = [{<<"From">>, From}] ++
               [{<<"To">>, join(To)} || To /= []] ++
               [{<<"Cc">>, join(Cc)} || Cc /= []] ++
               Headers,

    Headers2 = [{to_header_case(Header), Value} || {Header, Value} <- Headers1],
    Headers3 = case proplists:is_defined(<<"Date">>, Headers2) of
        false ->
            %% Append header; don't prepend.
            Date = list_to_binary(smtp_util:rfc5322_timestamp()),
            Headers2 ++ [{<<"Date">>, Date}];
        true ->
            Headers2
    end,

    MailFrom = extract_email_address(From),
    RcptTo   = lists:map(fun extract_email_address/1, To ++ Cc ++ Bcc),

    Head = [[Key, <<": ">>, Value, <<"\r\n">>] || {Key, Value} <- Headers3],
    Email = [Head, <<"\r\n">>, Body],

    {MailFrom, RcptTo, Email}.

%% @doc Returns a proplist that can be used as the 2nd argument to
%% gen_smtp_client:send/2,3 and gen_smtp_client:send_blocking/2.
-spec get_smtp_options(event()) -> [{atom(), term()}].
get_smtp_options(#event{smtp = #smtp{relay = Relay, port = Port,
                                     username = User, password = Pass}}) ->
    [{relay, Relay} || Relay /= undefined] ++
        [{port, Port}  || Port /= undefined] ++
        [{username, User} || User /= undefined] ++
        [{password, Pass} || Pass /= undefined].

%% @doc Returns a mail as a triple that can be used as the first argument to
%% gen_smtp_client:send/2,1 and gen_smtp_client:send_blocking/2.
%%
%% The returned mail is an error message with the original mail attached.
-spec build_error_mail(event(), OrigMail :: binary()) ->
    {MailFrom :: binary(), RcptTo :: [binary()], Email :: iodata()}.
build_error_mail(#event{error = #error{to = To, subject = Subject,
                                       body = Body}},
                 OrigMail) ->
    %% Error mail sender -- somewhat relates to the error smtp settings.
    {ok, ErrorFrom} = application:get_env(queuemailerl, error_from),

    %% The message part in a multipart/mixed email.
    MessagePart = {<<"text">>, <<"plain">>, [], [],  Body},

    %% The failing mail as an attachment
    Attachement = {<<"message">>, <<"rfc822">>, [],
                   [{<<"content-type-params">>, [{<<"name">>, <<"Mail">>}]},
                    {<<"dispisition">>, <<"attachment">>},
                    {<<"disposition-params">>, [{<<"filename">>, <<"Mail.eml">>}]}],
                   iolist_to_binary(OrigMail)},

    %% The envelope
    ErrorMail = mimemail:encode({<<"multipart">>, <<"mixed">>,
                                 [{<<"From">>, ErrorFrom}, {<<"To">>, To},
                                  {<<"Subject">>, Subject}],
                                 [],
                                 [MessagePart, Attachement]}),

    %% Wrap it in a tuple as expected by gen_smtp_client send functions.
    {extract_email_address(ErrorFrom), [extract_email_address(To)], ErrorMail}.

%% ------------------------------------------------------

%% Internal

parse_mail_part({Props}) ->
    From      = proplists:get_value(<<"from">>, Props),
    To        = proplists:get_value(<<"to">>, Props, []),
    Cc        = proplists:get_value(<<"cc">>, Props, []),
    Bcc       = proplists:get_value(<<"bcc">>, Props, []),
    {Headers} = proplists:get_value(<<"extra-headers">>, Props, []),
    Body      = proplists:get_value(<<"body">>, Props, <<>>),
    true = is_binary(From),
    true = lists:all(fun is_binary/1, To ++ Cc ++ Bcc),
    true = lists:all(fun ({_Key, Value}) -> is_binary(Value) end, Headers),
    true = is_binary(Body),
    #mail{from = From, to = To, cc = Cc, bcc = Bcc, headers = Headers, body = Body}.

parse_smtp_part({Props}) ->
    Relay    = proplists:get_value(<<"relay">>, Props),
    Port     = proplists:get_value(<<"port">>, Props),
    Username = proplists:get_value(<<"username">>, Props),
    Password = proplists:get_value(<<"password">>, Props),
    true = is_binary(Relay) or (Relay == undefined),
    true = is_integer(Port) or (Port == undefined),
    true = is_binary(Username) or (Username == undefined),
    true = is_binary(Password) or (Password == undefined),
    #smtp{relay = Relay, port = Port, username = Username, password = Password};
parse_smtp_part(undefined) ->
    %% All default
    #smtp{}.

parse_error_part({Props}) ->
    To      = proplists:get_value(<<"to">>, Props),
    Subject = proplists:get_value(<<"subject">>, Props),
    Body    = proplists:get_value(<<"body">>, Props),
    true = is_binary(To),
    true = is_binary(Subject),
    true = is_binary(Body),
    #error{to = To, subject = Subject, body = Body}.

%% @doc Returns a binary on the form `<<"<email@example.com>">>'.
extract_email_address(Bin) ->
    case re:run(Bin, <<"<(.*@.*)>$">>, [{capture, all_but_first, binary}]) of
        {match, [Email]} ->
            Email;
        nomatch ->
            match = re:run(Bin, <<"^\\S+@\\S+$">>, [{capture, none}]),
            <<"<", Bin/binary, ">">>
    end.

%% @doc Converts a header field name to "Header-Case", i.e. uppercase first char in each
%% dash-separated part.
to_header_case(Binary) ->
    String = string:to_lower(binary_to_list(Binary)),
    Tokens = string:tokens(String, "-"),
    HeaderCaseTokens = [[string:to_upper(First) | Rest] || [First|Rest] <- Tokens],
    HeaderCase = string:join(HeaderCaseTokens, "-"),
    list_to_binary(HeaderCase).

%% @doc Creates a comma + space separated list
-spec join([binary()]) -> binary().
join(Xs) -> join(Xs, <<>>).

join([X], Acc) -> <<Acc/binary, X/binary>>;
join([X|Xs], Acc) -> join(Xs, <<Acc/binary, X/binary, ", ">>).
