import "babel-polyfill";
import "./styles/bulma.css";
import "font-awesome-webpack";

const fb = require('./appfb');
const Elm = require('../elm/Main');
// inject bundled Elm app into div#main
const app = Elm.Main.embed(document.getElementById('main'));

const memberListener = fb.memberListener();
const lineItemlistener = fb.lineItemlistener();

fb.auth.onAuthStateChanged(function(user) {
    app.ports.auth.send(!!user);
});

app.ports.login.subscribe(user => {
    fb.auth.signInWithEmailAndPassword(user.email, user.password)
        .then(() => {
            memberListener.once('value').then(function(snapshot) {
                const members = snapshot.val()
                app.ports.membersRetrieved.send(Object.values(members));
            });
            lineItemlistener.once('value').then(function(snapshot) {
                const lineItems = snapshot.val()
                app.ports.lineItemsRetrieved.send(Object.values(lineItems));
            });
        })
        .catch(function() {
            app.ports.loginFailed.send();
        });
});

app.ports.logout.subscribe(user => {
    fb.auth.signOut();
});

app.ports.addMember.subscribe(member => {
    fb.addMember(member).catch(err => {
        console.error("addMember error:", err);
    });
});

app.ports.updateMember.subscribe(function(member) {
    fb.updateMember(member).catch(err => {
        console.error("updateMember error:", err);
    });
});

app.ports.addLineItem.subscribe(function(lineItem) {
    fb.addLineItem(lineItem).catch(err => {
        console.error("addLineItem error:", err);
    });
});

app.ports.updateLineItem.subscribe(function(lineItem) {
    fb.updateLineItem(lineItem).catch(err => {
        console.error("updateLineItem error:", err);
    });
});

app.ports.deleteLineItem.subscribe(function(lineItem) {
    fb.deleteLineItem(lineItem).catch(err => {
        console.error("deleteLineItem error:", err);
    });
});

memberListener.on("child_added", data => {
    const member = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.memberAdded.send(member);
});
memberListener.on("child_changed", data => {
    const member = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.memberUpdated.send(member);
});

lineItemlistener.on("child_added", data => {
    const lineItem = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.lineItemAdded.send(lineItem);
});
lineItemlistener.on("child_changed", data => {
    const member = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.lineItemUpdated.send(member);
});
lineItemlistener.on("child_removed", data => {
    const lineItem = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.lineItemDeleted.send(lineItem);
});
