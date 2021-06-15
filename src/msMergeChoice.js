import cytoscape from 'cytoscape';
import dagre from 'cytoscap-dagre';
import _ from 'underscore';
import * as styles from "./styles.js";

function randomArray(min_len, max_len, min_val, max_val) {
    /* Generate a random array of integers of random length.

       Steps:

       1. Generate a random integer n in range [min_len] to [max_len].
       This integer determines the length of generated random array.

       2. Generate an array of random integers of length n in the 
       range [min_val] to [max_val].
    */
    const arrlen = _.sample(min_len, max_len);
    return _.sample(_.range(min_val, max_val), arrlen);
}


function initialNodes() {

    const min_len = 5;
    const max_len = 10;
    const min_val = 5;
    const max_val = 50;
    
    return [
	{
	    group: "nodes",
	    data: {
		id: "$",
		lst: randomArray(min_len, max_len, min_val, max_val)
	    }
	},	
	{
	    group: "nodes",
	    data: {
		id: "split_$"
	    }
	}
    ];
}


function initialEdges(inodes) {

    return {
	group: "edges",
	data: {
	    source: inodes.filter("#$"),
	    target: inodes.filter("#split_$")
	}
    };
}

function init() {

    const initNodes = initialNodes();
    const initEdges = initialEdges(initNodes);

    return {
	graph: cytoscape({
	    container: document.getElementById('graph'),
	    autoungrabify: true,
	    autounselectify: true,
	    elements: {
		nodes: 
		edges: 
	    },	
	    style: styles.graph_style,	
	    layout: {
		name: 'cose'
	    }
	})
    };
}

const model = init();


