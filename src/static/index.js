import "babel-polyfill";
import "./styles/bulma.css";
import "font-awesome-webpack";

const fb = require('./appfb');
const Elm = require('../elm/Main');
// inject bundled Elm app into div#main
const app = Elm.Main.embed(document.getElementById('main'));



fb.auth.onAuthStateChanged(function(user) {
  app.ports.auth.send(!!user);
});

app.ports.login.subscribe(user => {
  fb.auth.signInWithEmailAndPassword(user.email, user.password)
  .catch(function() {
    app.ports.loginFailed.send();
  });
});

app.ports.logout.subscribe(user => {
  fb.auth.signOut();
});

app.ports.addMember.subscribe(member => {
  fb.addMember(member).then(function (response) {
    }
    , function (err) {
      console.error("addMember error:", err);
    });
});

app.ports.updateMember.subscribe(function (member) {
  fb.updateMember(member).then(function (response) {
  }, function (err) {
    console.error("updateMember error:", err);
  });
});

app.ports.addLineItem.subscribe(function (lineItem) {
  fb.addLineItem(lineItem).then(function (response) {
  }, function (err) {
    console.error("addLineItem error:", err);
  });
});

app.ports.updateLineItem.subscribe(function (lineItem) {
  fb.updateLineItem(lineItem).then(function (response) {
  }, function (err) {
    console.error("updateLineItem error:", err);
  });
});

app.ports.deleteLineItem.subscribe(function (lineItem) {
  console.log('delete')
  fb.deleteLineItem(lineItem).then(function (response) {
  }, function (err) {
    console.error("deleteLineItem error:", err);
  });
});


const memberListener = fb.memberListener();
memberListener.on("child_added", function (data) {
  const member = Object.assign({}, data.val(), { id: data.key });
  app.ports.memberAdded.send(member);
});
memberListener.on("child_changed", function (data) {
  const member = Object.assign({}, data.val(), { id: data.key });
  app.ports.memberUpdated.send(member);
});

const lineItemlistener = fb.lineItemlistener();
lineItemlistener.on("child_added", function (data) {
  const lineItem = Object.assign({}, data.val(), { id: data.key });
  app.ports.lineItemAdded.send(lineItem);
});
lineItemlistener.on("child_changed", function (data) {
  const member = Object.assign({}, data.val(), { id: data.key });
  app.ports.lineItemUpdated.send(member);
});
lineItemlistener.on("child_removed", function (data) {
  const lineItem = Object.assign({}, data.val(), { id: data.key });
  app.ports.lineItemDeleted.send(lineItem);
});
