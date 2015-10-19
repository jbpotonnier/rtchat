Zepto(function($){
    var websocket = new WebSocket('ws://localhost:3000');
    
    websocket.onopen = function(event) { console.log('Websocket opened'); };
    
    websocket.onclose = function(event) { console.log('Wesocket closed'); };
    
    websocket.onmessage = function(event) {
        $('.message_list').append($('<li>' + event.data + '</li>'))
    };
    
    websocket.onerror = function(event) { alert(evt) };
})
