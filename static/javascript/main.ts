var m: any; // TODO mythril type definitions

interface Property<T> {
  (valueToSet?: T): T;
}

function prop<T>(initialValue: T): Property<T> {
  return m.prop(initialValue);
}

// TODO namespace

class Message {
  recipient: Property<string>;
  text: Property<string>;

  constructor(data: { text: string }) {
    this.text = prop(data.text);
    this.recipient = prop('jb');
  }

  static list() {
    return m.request({
      method: "GET",
      url: "/message",
      type: Message
    });
  }
}

class Controller {
  messages: Property<Message[]>;
  inputText: Property<string>;

  constructor() {
    this.messages = Message.list();
    this.inputText = prop("");
    this.connectWebSocket();
  }

  connectWebSocket() {
    var websocket = new WebSocket('ws://localhost:3000');

    websocket.onopen = (event) => { console.log('Websocket opened'); };

    websocket.onclose = (event) => { console.log('Wesocket closed'); };

    websocket.onerror = (event) => { alert(event) };

    websocket.onmessage = (event) => {
      this.messages().push(new Message({ text: event.data }));
      m.redraw();
    };
  }

  messageChanged() {
    if (this.inputText().length !== 0) {
      m.request({
        method: "POST",
        url: "/message",
        data: new Message({ text: this.inputText() })
      });
      this.inputText('');
    }
  }
}

var view = (ctrl: Controller) =>
  m("html", [
    m("body", [
      m("ul",
        ctrl.messages().map(message => m("li", [message.text()]))
      ),
      m("input", {
        value: ctrl.inputText(),
        oninput: m.withAttr('value', ctrl.inputText),
        onchange: () => ctrl.messageChanged()
      })
    ])
  ]);


m.mount(document, {
  controller: Controller,
  view: view
});
