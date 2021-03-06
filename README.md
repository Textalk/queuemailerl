queuemailerl
============

[![Build Status](https://travis-ci.org/Textalk/queuemailerl.svg)](https://travis-ci.org/Textalk/queuemailerl)

Dispatches emails from a RabbitMQ queue to multiple SMTP servers.

Status: Work in progress.

Requirements:

* Erlang/OTP R16B02 or later. (`application:ensure_all_started/2` was added in R16B01.)
* RabbitMQ

Modus operandi
--------------

1. A message arrives on the message queue;
2. the process is spawned to send the email to the SMTP server;
  * on failure, the process waits a certain time (longer each time) and retries;
  * after the maximum number of retries, a message is sent to the error reporting
    address using the error reporting SMTP settings;
3. the message from the message queue is acknowledged i.e. removed from the
   message queue.

Queue message format
--------------------

The queued message contains an email and the full SMTP settings along with an
error reporting email address. It is formatted as a JSON object on the form:

```JSON
{"mail": {"from": "Alice <alice@example.com>",
          "to": ["Bob <bob@example.com>"],
          "cc": [],
          "bcc": [],
          "subject": "hello",
          "extra-headers": {"Reply-To": "John <john@example.com>"},
          "body": "Dear Bob,\n\nI just want to say hello.\n\nAlice"},
 "smtp": {"relay": "localhost",
          "port": 25,
          "username": "alice",
          "password": "d9Jeaoid9%ud4"},
 "error": {"to": "email-administrator@example.com",
           "subject": "Subject in error report mail",
           "body": "The message to be sent in the event of error"}}
```

Required fields:

 - `"mail/from"`
 - At least one of `"mail/to"`, `"mail/cc"` or `"mail/bcc"`
 - `"mail/body"`
 - `"smtp/relay"`
 - `"smtp/port"`
 - `"error/to"`
 - `"error/subject"`
 - `"error/body"`

All other fields are optional.

`"body"` in `"mail"` can optionally be a list of parts of the folloming format:

```JSON
{"headers": {"content-type": "text/plain",
             "content-encoding": "base64",
             "content-filename": "file.txt",
             "content-disposition": "attchement"},
 "body": "..."
}
```

All fields except `"body"` is optional in a body part declaration.

Where all the headers are optional and the body can be either a string or, if
the `"content-type"` is a multipart-type, a list of the same structure. In the
list case the body is made up of parts. This makes it possible to create a
recursively defined tree of multipart parts and non-multipart bodies as leaves.

`"body"` in `"mail"` can optionally be a list of parts of the following format:

```JSON
{"headers": {"content-type": "text/plain",
             "content-encoding": "base64",
             "content-filename": "file.txt",
             "content-disposition": "attchement"},
 "body": "..."
}
```

Where all the headers are optional and the body can be either a string or, if
the `"content-type"` is a multipart-type, a list of the same structure. In the
list case the body is made up of parts. This makes it possible to create a
recursively defined tree of multipart parts and non-multipart bodies as leaves.

Error handling
--------------

In the event of error in sending the e-mail the subproperties of `"error"` are
used. An email with the contents of `"body"` is sent to `"to"` with the subject
`"subject"` and the failing email attatched. If there is any useful information
about what went wrong, this is appended to `"body"`. The error message should be
a plain text string in UTF-8.

Typical errors:

* SMTP server not reachable
* SMTP server timeout
* Invalid SMTP username or password

Important note: In the case of an invalid message sent to queuemailerl there
will be no error mail sent.

Application settings
--------------------

The following `env` settings exist for the `queuemailerl` application:

* `rabbitmq_configs`: a list of RabbitMQ configurations, each a list of
  options: `{username, binary()} | {password, binary()} | {vhost, binary()} |
  {host, binary()} | {port, integer()}` where `vhost` is optional with
  `<<"/">>` as the default.
* `rabbitmq_queue`: name of the queue; default `<<"queuemailerl">>`.
* `retry_count`: optional; default 10.
* `retry_initial_delay`: The delay for the first retry. This is doubled for
  each retry. This is optional with a default value of 60000.
* `error_smtp`: A list of SMTP options for use when sending error reports. The
  options are the same as for the second argument to
  [`gen_smtp_client:send/2,3`](https://github.com/Vagabond/gen_smtp/).
* `error_from`: The sender of the error email.

Tests
-----

You can run the tests with `make tests`. RabbitMQ needs to be running on
localhost with full permissions for the user "test", password "test" on the
vhost "/test". (Hint: Look at the `.travis.yml` file for how this can be set
up.)
