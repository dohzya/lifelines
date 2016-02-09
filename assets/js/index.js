console.log("Bienvenue sur LifelineS")

var websocket;
var output;
var elTalking;

function doSend(message) {
  websocket.send(message);
}

function addElement(el) {
  var scrolled = (window.innerHeight + window.scrollY) < document.body.offsetHeight;
  output.appendChild(el);
  if (!scrolled) el.scrollIntoView();
}

function findParentChoice(el) {
  if (el.classList.contains("choice")) return el;
  return findParentChoice(el.parentNode);
}
function currentChoice(el) {
  el.classList.add("current-choice");
  el.onclick = function (e) {
    el.classList.remove("current-choice");
    el.onclick = undefined;
    var choice = findParentChoice(e.target);
    choice.classList.add("selected-choice");
    send(choice.dataset.name);
  };
}

function talking() {
  if (!elTalking) {
    elTalking = document.createElement("div");
    elTalking.className = `message talking`;
    elTalking.innerHTML = "<p>…</p>";
    addElement(elTalking);
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
  var elMsg = document.createElement("div");
  elMsg.className = `message ${kind}`;
  elMsg.innerHTML = content;
  addElement(elMsg);
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
  addElement(elChoices);
  currentChoice(elChoices);
}

function send(action) {
  var msg = {"action": action};
  websocket.send(JSON.stringify(msg));
}

function start(opts) {
  websocket = new WebSocket(opts.wsUri + location.search);
  output = opts.output;
  websocket.onopen = (evt) => {
    showMessage("<p>-- connecté -- </p>", "info");
  };
  websocket.onclose = (evt) => {
    showMessage("<p>-- déconnecté -- </p>", "info");
  };
  websocket.onmessage = (evt) => {
    var data = JSON.parse(evt.data);
    if (data.kind == "choices") showChoices(data.choices);
    else if (data.kind == "talking") talking();
    else showMessage(data.message, data.kind);
  };
  websocket.onerror = (evt) => {
    showMessage(evt.data, "<p>error</p>");
  };
}

window.startGame = start;
