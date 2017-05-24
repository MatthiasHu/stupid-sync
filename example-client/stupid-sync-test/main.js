"use strict";

var domain = "ws://stupidtabletop.ddns.net:39141/"

var nameInput; // name text input
var dataArea; // data text area

var socket = null; // web socket in use or null


function onLoad() {
  console.log("wöks!");

  nameInput = document.getElementById("name");
  dataArea = document.getElementById("data");
  nameInput.value = "stupid-sync-test/default";
  nameInput.oninput = reconnect;
  dataArea.oninput = dataInput;
  disableDataArea();

  reconnect();

  nameInput
}

// Try to connect to the sync object specified in nameInput,
// closing the previous connection if present.
function reconnect() {
  console.log("reconnecting.");

  if (socket != null) {
    disableDataArea();
    socket.close();
    socket = null;
  }

  socket = new WebSocket(domain + nameInput.value);
  socket.onopen = enableDataArea;
  socket.onerror = socketClosed;
  socket.onclose = socketClosed;
  socket.onmessage = function(e) {newData(e.data);}
}

function enableDataArea() {
  dataArea.disabled = false;
}
function disableDataArea() {
  dataArea.disabled = true;
}

function socketClosed() {
  disableDataArea();
  socket = null;
}

function newData(blob) {
  console.log("new data: " + blob);
  
  var reader = new FileReader();
  reader.onload = function() {
    dataArea.value = reader.result;
  }
  reader.onerror = function() {
    console.log("(error reading blob)");
  }
  reader.readAsText(blob);
}

function dataInput() {
  console.log("sending data.");
  socket.send(dataArea.value);
}
