'use strict';

const config = {
    apiKey: "AIzaSyDu1-L4rJ9DNI1KZ1JW-2reP61QFdAUegg",
    authDomain: "bookkeeping-e217c.firebaseapp.com",
    databaseURL: "https://bookkeeping-e217c.firebaseio.com",
    storageBucket: "bookkeeping-e217c.appspot.com",
    messagingSenderId: "577641269612"
};

const app = firebase.initializeApp(config);
const database = app.database();
const MEMBER_REFPATH = "members";
const LINEITEM_REFPATH = "line_items";

export const auth = app.auth();

export const members = {
    add: member => {
        return database
            .ref(MEMBER_REFPATH)
            .push(member);
    },

    update: member => {
        return database
            .ref(MEMBER_REFPATH + "/" + member.id)
            .set(member);
    },

    ref: database.ref(MEMBER_REFPATH)
}

export const lineItems = {
    add: lineItem => {
        return database
            .ref(LINEITEM_REFPATH)
            .push(lineItem);
    },

    update: lineItem => {
        return database
            .ref(LINEITEM_REFPATH + "/" + lineItem.id)
            .set(lineItem);
    },

    delete: lineItem => {
        return database
            .ref(LINEITEM_REFPATH + "/" + lineItem.id)
            .remove();
    },

    ref: database.ref(LINEITEM_REFPATH)
}
