var chat = {};

chat.Message = function (data) {
    this.text = m.prop(data.text);
    this.recipient = m.prop('jb');
};

chat.Message.list = function () {
    return m.request({method: "GET", url: "/message", type: chat.Message});
}


chat.state = {
    init: function () {
        chat.state.messages = chat.Message.list();
        chat.state.message = m.prop('');
        this.connectWebSocket();
    },

    
    connectWebSocket: function () {
        var websocket = new WebSocket('ws://localhost:3000');
        
        websocket.onopen = function(event) { console.log('Websocket opened'); };
        
        websocket.onclose = function(event) { console.log('Wesocket closed'); };
        
        websocket.onerror = function(event) { alert(evt) };
        
        websocket.onmessage = function(event) {
            chat.state.messages().push(new chat.Message({text: event.data}));
            m.redraw();
        };
    }
};

chat.controller = function () {
    chat.state.init();

    this.messageChanged = function () {
        if (chat.state.message().length !== 0) {
            m.request({method: "POST",
                       url: "/message",
                       data: new chat.Message({text: chat.state.message()})});
            chat.state.message('');
        }
    }
};

chat.view = (ctrl) => 
    m("html", [
        m("body", [
            m("ul", 
              chat.state.messages().map(message => m("li", [message.text()]))
             ),
            m("input", {value: chat.state.message(),
                        oninput: m.withAttr('value', chat.state.message),
                        onchange: ctrl.messageChanged})
        ])
    ]);

m.mount(document, {controller: chat.controller, view: chat.view});

