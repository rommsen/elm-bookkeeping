import "babel-polyfill";
import "./styles/bulma.css";
import "font-awesome-webpack";

import {
    auth,
    member,
    lineItem
} from "./appfb";

const Elm = require('../elm/Main');
// inject bundled Elm app into div#main
const app = Elm.Main.embed(document.getElementById('main'));

auth.onAuthStateChanged(function(user) {
    app.ports.auth.send(!!user);
});

app.ports.login.subscribe(user => {
    auth.signInWithEmailAndPassword(user.email, user.password)
        .then(() => {
            member.ref.once('value').then(function(snapshot) {
                const members = snapshot.val()
                app.ports.membersRetrieved.send(Object.values(members));
            });
            lineItem.ref.once('value').then(function(snapshot) {
                const lineItems = snapshot.val()
                app.ports.lineItemsRetrieved.send(Object.values(lineItems));
            });
        })
        .catch(function() {
            console.log('login failed', arguments);
            app.ports.loginFailed.send();
        });
});

app.ports.logout.subscribe(user => {
    auth.signOut();
});

app.ports.addMember.subscribe(member => {
    member.add(member).catch(err => {
        console.error("addMember error:", err);
    });
});

app.ports.updateMember.subscribe(function(member) {
    member.update(member).catch(err => {
        console.error("updateMember error:", err);
    });
});

app.ports.addLineItem.subscribe(function(lineItem) {
    lineItem.add(lineItem).catch(err => {
        console.error("addLineItem error:", err);
    });
});

app.ports.updateLineItem.subscribe(function(lineItem) {
    lineItem.update(lineItem).catch(err => {
        console.error("updateLineItem error:", err);
    });
});

app.ports.deleteLineItem.subscribe(function(lineItem) {
    lineItem.delete(lineItem).catch(err => {
        console.error("deleteLineItem error:", err);
    });
});

member.ref.on("child_added", data => {
    const member = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.memberAdded.send(member);
});
member.ref.on("child_changed", data => {
    const member = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.memberUpdated.send(member);
});

lineItem.ref.on("child_added", data => {
    const lineItem = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.lineItemAdded.send(lineItem);
});
lineItem.ref.on("child_changed", data => {
    const member = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.lineItemUpdated.send(member);
});
lineItem.ref.on("child_removed", data => {
    const lineItem = Object.assign({}, data.val(), {
        id: data.key
    });
    app.ports.lineItemDeleted.send(lineItem);
});
