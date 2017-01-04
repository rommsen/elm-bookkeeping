'use strict';
// Initialize Firebase
var config = {
  apiKey: "AIzaSyDu1-L4rJ9DNI1KZ1JW-2reP61QFdAUegg",
  authDomain: "bookkeeping-e217c.firebaseapp.com",
  databaseURL: "https://bookkeeping-e217c.firebaseio.com",
  storageBucket: "bookkeeping-e217c.appspot.com",
  messagingSenderId: "577641269612"
};

var app = firebase.initializeApp(config);
var database = app.database();
var MEMBER_REFPATH = "members";
var LINEITEM_REFPATH = "line_items";

function addMember(member){
  return database
    .ref(MEMBER_REFPATH)
    .push(member);
}

function updateMember(member){
  return database
    .ref(MEMBER_REFPATH + "/" + member.id)
    .set(member);
}

function memberListener(){
  return database.ref(MEMBER_REFPATH);
}

function addLineItem(lineItem){
  return database
    .ref(LINEITEM_REFPATH)
    .push(lineItem);
}

function updateLineItem(lineItem){
  return database
    .ref(LINEITEM_REFPATH + "/" + lineItem.id)
    .set(lineItem);
}


function deleteLineItem(lineItem){
  return database
    .ref(LINEITEM_REFPATH + "/" + lineItem.id)
    .remove();
}


function lineItemlistener(){
  return database.ref(LINEITEM_REFPATH);
}


module.exports = {
  addMember: addMember,
  updateMember:updateMember,
  memberListener: memberListener,
  addLineItem: addLineItem,
  updateLineItem: updateLineItem,
  deleteLineItem: deleteLineItem,
  lineItemlistener:lineItemlistener,
  auth: app.auth()
  };
