console.log("Bienvenue sur LifelineS")

var websocket;

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

function showMessage(opts, content, kind) {
  var elMsg = document.createElement("p");
  elMsg.className = `message ${kind}`;
  elMsg.innerHTML = content;
  opts.output.appendChild(elMsg);
}

function showChoices(opts, choices) {
  var elChoices = document.createElement("ul");
  elChoices.className = "choices";
  for (name in choices) {
    var elChoice = document.createElement("li");
    elChoice.className = "choice";
    elChoice.dataset.name = name;
    elChoice.innerHTML = choices[name];
    elChoices.appendChild(elChoice);
  }
  opts.output.appendChild(elChoices);
  currentChoice(elChoices);
}

function send(action) {
  var msg = {"action": action};
  websocket.send(JSON.stringify(msg));
}

function start(opts) {
  websocket = new WebSocket(opts.wsUri);
  websocket.onopen = (evt) => {
    showMessage(opts, "CONNECTED", "info");
    send("first");
  };
  websocket.onclose = (evt) => {
    showMessage(opts, "DISCONNECTED", "info");
  };
  websocket.onmessage = (evt) => {
    var data = JSON.parse(evt.data);
    if (data.kind == "choices") showChoices(opts, data.choices);
    else showMessage(opts, data.message, data.kind);
  };
  websocket.onerror = (evt) => {
    showMessage(opts, evt.data, "error");
  };
}

window.startGame = start;
