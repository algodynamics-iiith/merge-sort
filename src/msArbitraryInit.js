const dagre = require('dagre');

window.onload = () => {

    var app = Elm.MsArbitrary.init({
	node: document.getElementById('ms-arbitrary')
    });

    app.ports.sendMessage.subscribe(function(graphData) {

	g = dagre.graphlib.json.read(graphData);
	dagre.layout(g);	
	
	app.ports
	    .messageReceiver
	    .send(JSON.stringify(dagre.graphlib.json.write(g)));
    });

}
