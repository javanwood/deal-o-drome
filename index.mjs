import elm from "./build/elm.js"

var app = elm.Elm.Main.init();
console.log();
app.ports.log.subscribe(console.log);