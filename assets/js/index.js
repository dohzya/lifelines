console.log("Bienvenue sur LifelineS")

var websocket;
var output;
var elTalking;

function doSend(message) {
  websocket.send(message);
}

function currentChoice(el) {
  el.classList.add("current-choice");
  el.onclick = function (e) {
    el.classList.remove("current-choice");
    el.onclick = undefined;
    e.target.classList.add("selected-choice");
    send(e.target.dataset.name);
  };
}

function talking() {
  if (!elTalking) {
    elTalking = document.createElement("p");
    elTalking.className = `message talking`;
    elTalking.innerHTML = "...";
    output.appendChild(elTalking);
  }
}
function stopTalking() {
  if (elTalking) {
    elTalking.parentNode.removeChild(elTalking);
    elTalking = undefined;
  }
}

function showMessage(content, kind) {
  stopTalking();
  var elMsg = document.createElement("p");
  elMsg.className = `message ${kind}`;
  elMsg.innerHTML = content;
  output.appendChild(elMsg);
}

function showChoices(choices) {
  stopTalking();
  var elChoices = document.createElement("ul");
  elChoices.className = "choices";
  for (name in choices) {
    var elChoice = document.createElement("li");
    elChoice.className = "choice";
    elChoice.dataset.name = name;
    elChoice.innerHTML = choices[name];
    elChoices.appendChild(elChoice);
  }
  output.appendChild(elChoices);
  currentChoice(elChoices);
}

function send(action) {
  var msg = {"action": action};
  websocket.send(JSON.stringify(msg));
}

function start(opts) {
  websocket = new WebSocket(opts.wsUri);
  output = opts.output;
  websocket.onopen = (evt) => {
    showMessage("-- connecté -- ", "info");
  };
  websocket.onclose = (evt) => {
    showMessage("-- déconnecté -- ", "info");
  };
  websocket.onmessage = (evt) => {
    var data = JSON.parse(evt.data);
    if (data.kind == "choices") showChoices(data.choices);
    else if (data.kind == "talking") talking();
    else showMessage(data.message, data.kind);
  };
  websocket.onerror = (evt) => {
    showMessage(evt.data, "error");
  };
}

window.startGame = start;
