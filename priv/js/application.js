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
        var data    = JSON.parse(evt.data);
        var type    = data.type;
        var payload = data.payload;

        amplify.publish(type, payload);
    };

    /** Example listener which logs each published message. */
    amplify.subscribe("generic", function(data) {
        console.log(data);
    });

})();
