# piper

Piper, a system for treating the browser like an Erlang process.  Piper
is just a prototype and should not be used in production.

## Motivation

Piper was heavily inspired by Joe Armstrong's [Ezwebframe](https://github.com/joearms/ezwebframe).

Piper diverges from the approach in Ezwebframe in its handling of
calling JavaScript functions.  Rather than sending messages of desired
JavaScript function calls directly to the browser, Piper provides an
additional layer of indirection on the client side to allow handling of
messages similar to selective receive.

For example:

Given the following Erlang:

```erlang
Pid ! {client_count, 1}.
```

The following message is sent on the wire:

```javascript
{ type: "client_count", message: "1" }
```

Followed by the following message published via AmplifyJS:

```javascript
amplify.publish("client_count", 1);
```

This allows multiple consumers in your JavaScript application to
subscribe to these messages and respond to them accordingly.

Piper also provides a channel for publishing events which should be sent
to the server.

```javascript
amplify.publish("server", { message: boo });
```

## License

Copyright (c) 2012 Christopher Meiklejohn.  All Rights Reserved.

Piper is released under the Apache 2 License. See LICENSE file for more information.
