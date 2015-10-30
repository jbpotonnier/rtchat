var chat = {};

chat.Message = function (data) {
    this.text = m.prop(data.text);
};

chat.Messages = Array;

chat.vm = {
    init: function () {
        chat.vm.messages = new chat.Messages();

        var websocket = new WebSocket('ws://localhost:3000');
        
        websocket.onopen = function(event) { console.log('Websocket opened'); };
        
        websocket.onclose = function(event) { console.log('Wesocket closed'); };
        
        websocket.onerror = function(event) { alert(evt) };
        
        websocket.onmessage = function(event) {
            chat.vm.messages.push(new chat.Message({text: event.data}));
            m.redraw();
        };
    }
};

chat.controller = function () {
    chat.vm.init();
};

chat.view = function () {
    return m("html", [
        m("body", [
            m("ul", 
              chat.vm.messages.map(function(message) {
                  return m("li", [message.text()]);
              })
             )
        ])
    ]);
};


m.mount(document, {controller: chat.controller, view: chat.view});

