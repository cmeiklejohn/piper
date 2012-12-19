(function() {

    /** Create a new websocket */
    ws = new WebSocket("ws://127.0.0.1:8080/websocket");

    /** Log initially when the socket is opened. */
    ws.onopen = function() {
        console.log("Connected!");
    };

    /** Log when the socket is closed. */
    ws.onclose = function() {
        console.log("Connection closed!");
    };

    /** On each message, publish an event of the message type.
     **
     ** Application developers should subscribe to these events with
     ** application specific behaviour. */
    ws.onmessage = function(evt) {
        amplify.publish("client", JSON.parse(evt.data));
    };

    /** Listener which subscribes to server events, and sends them back
     ** over the web socket. */
    amplify.subscribe("server", function(data) {
        ws.send(JSON.stringify(data));
    });

    /** Example listener which logs each published message. */
    amplify.subscribe("client", function(data) {
        console.log(data);
    });

    /** Example polling routine which sends messages back to the server,
     ** and logs the respones that come back when messages are sent back.
     */
    setInterval(function() {
        amplify.publish("server", { message: "count_clients" });
    }, 1000);

})();
