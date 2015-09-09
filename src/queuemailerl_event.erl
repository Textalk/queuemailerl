%% @doc Abstract datatype for an event.
-module(queuemailerl_event).

-export([parse/1, get_mail/1, get_smtp_options/1, build_error_mail/1]).
-export_type([event/0]).

-record(part_headers, {
          content_type               = <<"text/plain">> :: binary(),
          content_type_params        = []               :: [{binary(), binary()}],
          content_disposition        = <<"inline">>     :: binary(),
          content_disposition_params = []               :: [{binary(), binary()}],
          encoding                   = undefined        :: binary() | 'undefined'
         }).
-record(part, {
          headers :: #part_headers{},
          body    :: binary() | [#part{}]
         }).
-record(mail, {
          from    :: binary(),
          to      :: [binary()],
          cc      :: [binary()],
          bcc     :: [binary()],
          headers :: [{binary(), binary()}],
          body    :: binary() | [#part{}]
         }).
-record(smtp, {
          relay    :: binary(),
          port     :: non_neg_integer(),
          username :: binary(),
          password :: binary()
         }).
-record(error, {
          to      :: binary(),
          subject :: binary(),
          body    :: binary()
         }).
-record(event, {
          mail  :: #mail{},
          smtp  :: #smtp{},
          error :: #error{}
         }).

-opaque event() :: #event{}.

%% @doc Parses a raw JSON event and raises an error if the event is incorrect.
-spec parse(binary()) -> {ok, event()} | {error, term()}.
parse(RawEvent) ->
    try
        {Props} = jiffy:decode(RawEvent),
        MailRecord = parse_mail(proplists:get_value(<<"mail">>, Props)),
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

parse_content_specs(ContentDisposition) ->
    case binary:split(ContentDisposition, <<";">>) of
        [Disp] -> {Disp, []};
        [Disp, Parameters] ->
            Params = lists:map(
                       fun (M) ->
                               case re:split(M, <<"[[:space:]]*=[[:space:]]*">>, [{parts, 2}]) of
                                   [Key] -> {Key, true};
                                   [Key, Value] -> {Key, Value}
                               end
                       end,
                       re:split(Parameters, <<"[[:space:]]*(?=[^\\\\]);[[:space:]]*">>)),
            {Disp, Params}
    end.

parse_body({Part}) ->
    Headers0 = case proplists:get_value(<<"headers">>, Part, {[]}) of
                     {H0} -> H0;
                     [] -> []
                 end,
    Body0 = proplists:get_value(<<"body">>, Part),
    Headers1 = lists:foldl(
                 fun ({<<"content-type">>, ContentType}, PH)
                       when is_binary(ContentType) ->
                         Params0 = PH#part_headers.content_type_params,
                         {CType, Params1} = parse_content_specs(ContentType),
                         [CTMajor, CTMinor] = binary:split(CType, <<"/">>),
                         PH#part_headers{content_type = {CTMajor, CTMinor},
                                         content_type_params = Params0 ++ Params1};
                     ({<<"content-encoding">>, Encoding}, PH)
                       when is_binary(Encoding) ->
                         PH#part_headers{encoding = Encoding};
                     ({<<"content-filename">>, Filename}, PH)
                       when is_binary(Filename) ->
                         QFilename = <<"\"", Filename/binary, "\"">>,
                         Params = PH#part_headers.content_disposition_params,
                         Param = {<<"filename">>, QFilename},
                         PH#part_headers{
                           content_disposition = <<"attachment">>,
                           content_disposition_params = [Param|Params]
                          };
                     ({<<"content-disposition">>, ContentDisp}, PH)
                       when is_binary(ContentDisp) ->
                         Params0 = PH#part_headers.content_disposition_params,
                         {Disp, Params1} = parse_content_specs(ContentDisp),
                         PH#part_headers{content_disposition = Disp,
                                         content_disposition_params = Params0 ++ Params1}
                 end, #part_headers{}, Headers0),
    Body1 = case is_binary(Body0) of
                %% @todo (2015-09-07) Add sanity check to make sure that content-type correctly set
                %% depending on the following cases. If false content type needs to be multipart, if
                %% true it needs to be not multipart.
                true ->
                    case Headers1#part_headers.encoding of
                        <<"base64">> -> base64:decode(Body0);
                        _ -> Body0
                    end;
                false ->
                    lists:map(fun parse_body/1, Body0)
            end,
    #part{headers = Headers1, body = Body1}.

parse_mail({Props}) ->
    From    = proplists:get_value(<<"from">>, Props),
    To      = proplists:get_value(<<"to">>, Props, []),
    Cc      = proplists:get_value(<<"cc">>, Props, []),
    Bcc     = proplists:get_value(<<"bcc">>, Props, []),
    Subject = proplists:get_value(<<"subject">>, Props, undefined),
    Headers = case proplists:get_value(<<"extra-headers">>, Props, {[]}) of
                  {H0} -> H0;
                  [] -> []
              end,
    Body0   = proplists:get_value(<<"body">>, Props),
    true = is_binary(From),
    true = lists:all(fun is_binary/1, To ++ Cc ++ Bcc),
    true = lists:all(fun ({_Key, Value}) -> is_binary(Value) end, Headers),
    Body1 = case is_binary(Body0) of
                true ->
                    Body0;
                false ->
                    lists:map(fun parse_body/1, Body0)
            end,
    Headers1 = case Subject of
                   undefined ->
                       Headers;
                   _ when is_binary(Subject) ->
                       lists:keystore(<<"Subject">>, 1, Headers, {<<"Subject">>, Subject})
               end,
    Mail = #mail{
              from = From,
              to = To,
              cc = Cc,
              bcc = Bcc,
              headers = Headers1,
              body = Body1
             },
    Mail.

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

build_mail_part(#part{body = Body0,
                      headers = #part_headers{
                                   content_type = ContentType,
                                   content_type_params = CTParams,
                                   content_disposition = ContentDisp,
                                   content_disposition_params = CDParams0,
                                   encoding = Encoding
                                  }
                     }) ->

    {CTMajor, CTMinor} = ContentType,
    Body1 = case is_binary(Body0) of
                true -> Body0;
                false -> lists:map(fun build_mail_part/1, Body0)
            end,

    CDParams1 = lists:map(fun ({K, true}) -> K;
                                          ({K, V}) -> <<K/binary, "=", V/binary>>
                                      end, CDParams0),
    CDParams2 = lists:foldl(fun (Param, Rest) ->
                                    case Rest of
                                        <<>> -> Param;
                                        _ -> <<Param/binary, ";", Rest/binary>>
                                    end
                            end,
                            <<>>,
                            CDParams1),

    ContentDisposition = case CDParams2 of
                             <<>> -> ContentDisp;
                             _ -> <<ContentDisp/binary, "; ", CDParams2/binary>>
                         end,
    Headers = case Encoding of
                  undefined -> [];
                  _ -> [{<<"Content-Transfer-Encoding">>, Encoding}]
              end,

    {CTMajor, CTMinor, Headers,
     [{<<"content-type-params">>, CTParams},
      {<<"disposition">>, ContentDisposition}],
     Body1}.

%% @doc See doc for get_mail/1.
-spec build_mail(#mail{}) ->
    {MailFrom :: binary(), RcptTo :: [binary()], Email :: binary()}.
build_mail(#mail{from = From, to = To, cc = Cc, bcc = Bcc,
                 headers = Headers, body = Body0}) ->
    CType = proplists:get_value(<<"Content-Type">>, Headers, undefined),
    Headers0 = proplists:delete(<<"Content-Type">>, Headers),
    Headers1 = [{<<"From">>, From}] ++
               [{<<"To">>, join(To)} || To /= []] ++
               [{<<"Cc">>, join(Cc)} || Cc /= []] ++
               Headers0,

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

    {CTMajor, CTMinor} = case CType of
                             undefined -> case is_binary(Body0) of
                                              true -> {<<"text">>, <<"plain">>};
                                              false -> {<<"multipart">>, <<"mixed">>}
                                          end;
                             _ when is_binary(CType) ->
                                 [Maj, Min] = binary:split(CType, <<"/">>),
                                 {Maj, Min}
                         end,
    Body1 = case is_binary(Body0) of
                true ->
                    Body0;
                false ->
                    [build_mail_part(Part) || Part <- Body0]
            end,

    Email = mimemail:encode({CTMajor, CTMinor, Headers3, [], Body1}),

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
