const GRAPH_NODE_BG = {
    unpicked: '#B2EBF2',
    picked: '#A5D6A7',
    candidate: '#009688'
};

const GRAPH_EDGE_BG = {
    unpicked: '#E0E0E0',
    picked: '#757575',
    candidate: '#FFD54F'
};


export const graph_style = [
    {
	selector: 'node',
	style: {
	    'background-color': (el) => {		
		if( el.classes().includes('edge-picked') ||
		    el.classes().includes('selected')
		  ){
		    return GRAPH_NODE_BG.picked;
		}
		else {
		    return GRAPH_NODE_BG.unpicked;
		}
	    },
	    'label': 'data(id)',
	    'text-valign': 'center',
	    'text-halign': 'center'
	}
    },
    {
	selector: 'edge',
	style: {
	    'width': 5,
	    'line-color': (el) => {
		if( el.classes().includes('picked') ){
		    return GRAPH_EDGE_BG.picked;
		}
		else {
		    if( el.classes().includes('candidate') ){
			return GRAPH_EDGE_BG.candidate;
		    }
		    else {
			return GRAPH_EDGE_BG.unpicked;
		    }
		}
	    },
	    'curve-style': 'bezier'
	}
    }
];


export const tree_style = [
    {
	selector: 'node',
	style: {
	    'background-color': (el) => {		
		if( el.classes().includes('edge-picked') ){
		    return GRAPH_NODE_BG.picked;
		}
		else {
		    return GRAPH_NODE_BG.unpicked;
		}
	    },
	    'label': 'data(id)',
	    'text-valign': 'center',
	    'text-halign': 'center'
	}
    },
    {
	selector: 'edge',
	style: {
	    'width': 5,
	    'line-color': (el) => {
		if( el.classes().includes('picked') ){
		    return GRAPH_EDGE_BG.picked;
		}
		else {
		    return GRAPH_EDGE_BG.unpicked;
		}
	    },
	    'curve-style': 'bezier'
	}
    }
];
