module V1.Converter exposing (toLg, toLgNode)

import V1.MergeSortGraph as MG
import V1.LayoutGraph as LG
import Graph as G

toLgNode : (G.Node MG.Node) -> LG.Node
toLgNode mgn = 
    LG.node (String.fromInt mgn.id) (MG.getLabel mgn.label)

toLg : MG.MSGraph -> LG.LayoutGraph
toLg mg =
    let nodes = mg |> G.nodes |> (List.map toLgNode)
 
        edges = mg |> G.edges
                   |> List.map (\e -> LG.edge (String.fromInt e.from) (String.fromInt e.to))
    in
        LG.fromNodesAndEdges nodes edges
