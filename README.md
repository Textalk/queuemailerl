queuemailerl
============

Dispatches emails from a RabbitMQ queue to multiple SMTP servers.

Status: Planning.

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
{"from": "\"Alice\" <alice@example.com>",
 "to": ["\"Bob\" <bob@example.com>"],
 "cc": [],
 "bcc": [],
 "headers": {"Subject", "hello",
             "Reply-To": "\"John\" <john@example.com>"},
 "body": "Dear Bob,\n\nI just want to say hello.\n\nYour friend,\n\nAlice",
 "relay": "localhost",
 "port": 23,
 "username": "alice",
 "password": "d9Jeaoid9%ud4",
 "error-to": "email-administrator@example.com",
 "error-subject": "Subject in error report email",
 "error-body": "The message to be sent to error-to in event of error"}
```

Error handling
--------------

*In the event of error* the `"error-body"` is sent to `"error-to"` with the
subject `"error-subject"` and the failing email attatched. If there is any
useful information about what went wrong, this is appended to
`"error-body"`. The error message should be a plain text string in UTF-8.

Typical errors:

* SMTP server not reachable
* SMTP server timeout
* Invalid SMTP username or password

Application settings
--------------------

* Error reporting SMTP settings
* RabbitMQ settings

Details to be defined.
