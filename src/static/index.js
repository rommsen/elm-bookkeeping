// pull in desired CSS/SASS files
require( './styles/bulma.css' );
require("font-awesome-webpack");

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );

var fb = require('./appfb')


app.ports.addMember.subscribe(function (member) {
  fb.addMember(member).then(function (response) {
  }, function (err) {
    console.log("error:", err);
  });
});

app.ports.updateMember.subscribe(function (member) {
  fb.updateMember(member).then(function (response) {
  }, function (err) {
    console.log("error:", err);
  });
});

app.ports.addLineItem.subscribe(function (lineItem) {
  fb.addLineItem(lineItem).then(function (response) {
  }, function (err) {
    console.log("error:", err);
  });
});

app.ports.updateLineItem.subscribe(function (lineItem) {
  console.log('update',lineItem)
  fb.updateLineItem(lineItem).then(function (response) {
  }, function (err) {
    console.log("error:", err);
  });
});

app.ports.deleteLineItem.subscribe(function (lineItem) {
  console.log('delete')
  fb.deleteLineItem(lineItem).then(function (response) {
  }, function (err) {
    console.log("error:", err);
  });
});


var memberListener = fb.memberListener();
memberListener.on("child_added", function (data) {
  var member = Object.assign({}, data.val(), { id: data.key });
  app.ports.memberAdded.send(member);
});
memberListener.on("child_changed", function (data) {
  var member = Object.assign({}, data.val(), { id: data.key });
  app.ports.memberUpdated.send(member);
});

var lineItemlistener = fb.lineItemlistener();
lineItemlistener.on("child_added", function (data) {
  var lineItem = Object.assign({}, data.val(), { id: data.key });
  app.ports.lineItemAdded.send(lineItem);
});
lineItemlistener.on("child_changed", function (data) {
  var member = Object.assign({}, data.val(), { id: data.key });
  app.ports.lineItemUpdated.send(member);
});
lineItemlistener.on("child_removed", function (data) {
  var lineItem = Object.assign({}, data.val(), { id: data.key });
  console.log('remnnnn', lineItem);
  app.ports.lineItemDeleted.send(lineItem);
});