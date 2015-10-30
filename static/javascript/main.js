var chat = {};

chat.Message = function (data) {
    this.text = m.prop(data.text);
};

chat.Messages = Array;

chat.state = {
    init: function () {
        chat.state.messages = new chat.Messages();

        var websocket = new WebSocket('ws://localhost:3000');
        
        websocket.onopen = function(event) { console.log('Websocket opened'); };
        
        websocket.onclose = function(event) { console.log('Wesocket closed'); };
        
        websocket.onerror = function(event) { alert(evt) };
        
        websocket.onmessage = function(event) {
            chat.state.messages.push(new chat.Message({text: event.data}));
            m.redraw();
        };
    }
};

chat.controller = function () {
    chat.state.init();
};

chat.view = () => 
    m("html", [
        m("body", [
            m("ul", 
              chat.state.messages.map(message => m("li", [message.text()]))
             )
        ])
    ]);

m.mount(document, {controller: chat.controller, view: chat.view});

