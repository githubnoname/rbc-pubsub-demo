
var ws = null;

function connect(){
    if(!("WebSocket" in window)){
        alert("This browser doesn't support websockets");
        return;
    }
    ws = new WebSocket("ws://" + window.location.host + "/ws");
    ws.onopen = function(){
        addDbg("Connected!");
        loadRooms()
    }
    ws.onmessage = function(e){
        addDbg(e.data);
        msg = JSON.parse(e.data);
        switch(msg.action) {
        case "list_channels":
            updateRooms(msg.channels);
            break;
        case "subscribe":
            if(msg.result == "ok")
                setRoom(msg.channel);
        }
    }
    var closeFun = function(){
        ws = null;
        addDbg("Disconnected!");
    }
    ws.onclose = closeFun;
    ws.onerror = closeFun;
}

function clearOptions(box) {
    while(box.options.length > 0)
        box.remove(0);
}

function addOptions(box, values){
    for(x of values){
        var opt = document.createElement("option");
        opt.text = x;
        box.add(opt)
    }
}

function updateRooms(channels){
    var room = document.getElementById('room');
    room.disabled = false;
    clearOptions(room);
    addOptions(room, channels);
    if(currentRoom() == "NIL" && channels.length > 0)
        subscribe(channels[0]);
}

function addMsg(from, message){
    document.getElementById('chat').value += from + ": " + message + "\n";
}

function addDbg(message){
    document.getElementById('dbg').value += message + "\n";
}

function selectedRoom(){
    var room = document.getElementById('room');
    return room.options[room.selectedIndex].text;
}

function currentRoom(){
    return document.getElementById('current_room').innerHTML;
}

function userName(){
    return document.getElementById('username').value;
}

function setRoom(room){
    document.getElementById('current_room').innerHTML = room;
}


// user actions

function changeRoom(){
    var room = currentRoom();
    var nextRoom = selectedRoom();
    if(room != nextRoom){
        if(room != "NIL")
            unsubscribe(room);
        subscribe(nextRoom);
    }
}

function loadRooms(){
    listChannels();
}

function sendMessage(){
    var message = document.getElementById('message');
    var room = currentRoom();
    var name = userName();
    if(room != "NIL"){
        publish(room, name, message.value);
        message.value="";
    }
    return false;
}

// pubsub API

function send_json(data){
    if(ws == null)
        addDbg("Not connected");
    else{
        var json = JSON.stringify(data);
        ws.send(json);
    }
}

function listChannels(){
    var msg = {
        action: "list_channels",
    };
    send_json(msg);
}

function subscribe(channel){
    var msg = {
        action: "subscribe",
        channel: channel,
    };
    send_json(msg);
}

function unsubscribe(channel){
    var msg = {
        action: "unsubscribe",
        channel: channel,
    };
    send_json(msg);
}

function publish(Channel, User, Data){
    var msg = {
        action: "publish",
        channel: Channel,
        message: {
            sender: User,
            data: Data,
        },
    };
    send_json(msg);
}
