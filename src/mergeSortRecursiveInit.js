const dagre = require('dagre');

window.onload = () => {

    var app = Elm.MergeSortRecursive.init({
	node: document.getElementById('ms-recursive')
    });

    app.ports.sendMessage.subscribe(function(graphData) {

	g = dagre.graphlib.json.read(graphData);
	dagre.layout(g);	
	
	app.ports
	    .messageReceiver
	    .send(JSON.stringify(dagre.graphlib.json.write(g)));
    });

}
