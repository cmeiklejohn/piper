# piper

Piper, a system for treating the browser like an Erlang process.

## Motivation

Piper was heavily inspired by Joe Armstrong's [EZWebFrame](https://github.com/joearms/ezwebframe).

Piper diverges from the approach in EZWebFrame in its handling of
calling JavaScript objects.  Rather than sending messages of desired
JavaScript function calls directly to the browser, Piper provides an
additional layer of indirection on the client side to allow handling of
messages similar to selective receive.

For example:

Given the following Erlang:

```erlang
Pid ! {client_count, 1}.
```

The following message is published via AmplifyJS:

```javascript
amplify.publish(client_count, 1);
```

This allows multiple consumers in your JavaScript application to
subscribe to these messages and respond to them accordingly.

## License

Copyright (c) 2012 Christopher Meiklejohn.  All Rights Reserved.

Piper is released under the Apache 2 License. See LICENSE file for more information.
