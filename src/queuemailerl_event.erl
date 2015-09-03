%% @doc Abstract datatype for an event.
-module(queuemailerl_event).

-export([parse/1, get_mail/1, get_smtp_options/1, build_error_mail/1]).
-export_type([event/0]).

-record(part_headers, {
          content_type = <<"text/plain">> :: binary(),
          content_disposition = <<"inline">> :: binary()
         }).
-record(part, {headers :: #part_headers{},
               body :: binary()}).
-record(mail, {from :: binary(),
               to :: [binary()],
               cc :: [binary()],
               bcc :: [binary()],
               headers :: [{binary(), binary()}],
               body :: {'plain', binary()} | {'parts', [#part{}]}
              }).
-record(smtp, {relay, port, username, password}).
-record(error, {to, subject, body}).
-record(event, {mail, smtp, error}).

-opaque event() :: #event{}.

%% @doc Parses a raw JSON event and raises an error if the event is incorrect.
-spec parse(binary()) -> {ok, event()} | {error, term()}.
parse(RawEvent) ->
    try
        {Props} = jiffy:decode(RawEvent),
        MailRecord = parse_mail_part(proplists:get_value(<<"mail">>, Props)),
        Mail = build_mail(MailRecord),
        #event{mail = Mail,
               smtp = parse_smtp_part(proplists:get_value(<<"smtp">>, Props)),
               error = parse_error_part(proplists:get_value(<<"error">>, Props))}
    of
        Event -> {ok, Event}
    catch
        error:Reason ->
            {error, Reason, erlang:get_stacktrace()}
    end.

%% @doc Returns a triple that can as the first argument to
%% gen_smtp_client:send/2,1 and gen_smtp_client:send_blocking/2.
get_mail(#event{mail = Mail}) ->
    Mail.

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
-spec build_error_mail(event()) ->
    {MailFrom :: binary(), RcptTo :: [binary()], Email :: iodata()}.
build_error_mail(#event{error = #error{to = To, subject = Subject,
                                       body = Body},
                        mail = {_, _, OrigMail}}) ->
    %% Error mail sender -- somewhat relates to the error smtp settings.
    {ok, ErrorFrom} = application:get_env(queuemailerl, error_from),

    %% The message part in a multipart/mixed email.
    MessagePart = {<<"text">>, <<"plain">>, [], [], Body},

    %% The failing mail as an attachment
    Attachement = {<<"message">>, <<"rfc822">>, [],
                   [{<<"content-type-params">>, [{<<"name">>, <<"Mail">>}]},
                    {<<"dispisition">>, <<"attachment">>},
                    {<<"disposition-params">>, [{<<"filename">>, <<"Mail.eml">>}]}],
                   OrigMail},

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

parse_parts({Part}) ->
    {Headers0} = proplists:get_value(<<"headers">>, Part, {[]}),
    Body = proplists:get_value(<<"body">>, Part),
    Headers1 = lists:foldl(fun ({<<"content-type">>, ContentType}, PH)
                                 when is_binary(ContentType) ->
                                   PH#part_headers{content_type = ContentType};
                               ({<<"content-disposition">>, ContentDisp}, PH)
                                     when is_binary(ContentDisp) ->
                                   PH#part_headers{content_disposition = ContentDisp}
                           end, #part_headers{}, Headers0),
    #part{body = Body, headers = Headers1}.

parse_mail_part({Props}) ->
    From      = proplists:get_value(<<"from">>, Props),
    To        = proplists:get_value(<<"to">>, Props, []),
    Cc        = proplists:get_value(<<"cc">>, Props, []),
    Bcc       = proplists:get_value(<<"bcc">>, Props, []),
    {Headers} = proplists:get_value(<<"extra-headers">>, Props, []),
    Body0     = proplists:get_value(<<"body">>, Props, <<>>),
    Parts     = proplists:get_value(<<"parts">>, Props, []),
    true = is_binary(From),
    true = lists:all(fun is_binary/1, To ++ Cc ++ Bcc),
    true = lists:all(fun ({_Key, Value}) -> is_binary(Value) end, Headers),
    Body1 = case Parts of
                [] when size(Body0) /= 0 -> {plain, Body0};
                [] -> error({bad_mail, <<"Mail has no body">>});
                _ when size(Body0) == 0 -> {parts, [parse_parts(Part) || Part <- Parts]};
                _ -> error({bad_mail, <<"Mail contains both parts and body">>})
             end,
    #mail{from = From,
          to = To,
          cc = Cc,
          bcc = Bcc,
          headers = Headers,
          body = Body1}.

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

build_mail_part(#part{body = Body,
                      headers = #part_headers{
                                   content_type = ContentType,
                                   content_disposition = ContentDisp
                                  }
                     }) ->
    [CTMajor, CTMinor0] = binary:split(ContentType, <<"/">>),
    {CTMinor1, CTParams0} = case binary:match(CTMinor0, <<";">>) of
                                   nomatch ->
                                       {CTMinor0, <<>>};
                                   _ ->
                                       [CTMinor2, Params] = binary:split(CTMinor0, <<";">>),
                                       {CTMinor2, Params}
                               end,
    CTParams1 = lists:foldl(fun (Param, Acc) ->
                                        case Param of
                                            <<>> -> Acc;
                                            New -> [New | Acc]
                                        end
                                end, [],
                                re:split(CTParams0, <<";">>, [])),
    CTParams2 = lists:map(
                  fun (Param) ->
                          [Key, Value] = re:split(Param,
                                                  <<"[[:space:]]*=[[:space:]]*">>,
                                                  [{parts, 2}]),
                          {Key, Value}
                  end,
                  CTParams1),
    {CTMajor, CTMinor1, [],
     [{<<"disposition">>, ContentDisp},
      {<<"content-type-params">>, CTParams2}],
     Body}.

%% @doc See doc for get_mail/1.
-spec build_mail(#mail{}) ->
    {MailFrom :: binary(), RcptTo :: [binary()], Email :: binary()}.
build_mail(#mail{from = From, to = To, cc = Cc, bcc = Bcc,
                 headers = Headers, body = Body}) ->
    Headers1 = [{<<"From">>, From}] ++
               [{<<"To">>, join(To)} || To /= []] ++
               [{<<"Cc">>, join(Cc)} || Cc /= []] ++
               Headers,

    Headers2 = [{to_header_case(Header), Value} || {Header, Value} <- Headers1],
    Headers3 = case proplists:is_defined(<<"Date">>, Headers2) of
        false ->
            %% Append header; don't prepend.
            Date = unicode:characters_to_binary(smtp_util:rfc5322_timestamp()),
            Headers2 ++ [{<<"Date">>, Date}];
        true ->
            Headers2
    end,

    MailFrom = extract_email_address(From),
    RcptTo   = lists:map(fun extract_email_address/1, To ++ Cc ++ Bcc),

    Email = case Body of
                {plain, BodyText} ->
                    mimemail:encode({<<"text">>, <<"plain">>, Headers3, [], BodyText});
                {parts, Parts} ->
                    mimemail:encode({<<"multipart">>, <<"mixed">>,
                                     Headers3, [],
                                     [build_mail_part(Part) || Part <- Parts]})
            end,

    {MailFrom, RcptTo, Email}.

%% @doc Returns a binary on the form `<<"<email@example.com>">>'.
extract_email_address(Bin) ->
    case re:run(Bin, <<"<.*@.*>$">>, [{capture, all, binary}]) of
        {match, [Email]} ->
            Email;
        nomatch ->
            match = re:run(Bin, <<"^\\S+@\\S+$">>, [{capture, none}]),
            <<"<", Bin/binary, ">">>
    end.

%% @doc Converts a header field name to "Header-Case", i.e. uppercase first char in each
%% dash-separated part.
to_header_case(Binary) ->
    String = string:to_lower(unicode:characters_to_list(Binary)),
    Tokens = string:tokens(String, "-"),
    HeaderCaseTokens = [[string:to_upper(First) | Rest] || [First|Rest] <- Tokens],
    HeaderCase = string:join(HeaderCaseTokens, "-"),
    unicode:characters_to_binary(HeaderCase).

%% @doc Creates a comma + space separated list
-spec join([binary()]) -> binary().
join(Xs) -> join(Xs, <<>>).

join([X], Acc) -> <<Acc/binary, X/binary>>;
join([X|Xs], Acc) -> join(Xs, <<Acc/binary, X/binary, ", ">>).
