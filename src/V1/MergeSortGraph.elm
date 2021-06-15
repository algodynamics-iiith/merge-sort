module V1.MergeSortGraph exposing (..)

import Graph as G exposing (Graph)
import V1.Function as F exposing (Function)
import IntDict

type Node
    = FuncNode String (MSFunc Int)
    | DataNode Data

getLabel : Node -> String
getLabel n = 
    case n of
        DataNode d -> d |> List.map String.fromInt |> String.join " "
        FuncNode l f -> l

type MSFunc a = Merge (Function (List a -> List a -> List a))
              | Split (Function (List a -> Int -> (List a, List a)))
              | MS (Function (List a -> List a))

type alias Data = List Int

merge : List Int -> List Int -> List Int
merge a b = b

ms : List Int -> List Int
ms l = l

split : List Int -> Int -> (List Int, List Int)
split l i = (l, l)

startComputation : MSGraph
startComputation = 
    G.fromNodesAndEdges 
        [ G.Node 1 (DataNode [10, 30, 20, 40])
        , G.Node 2 (FuncNode "MS" <| MS (F.prim ms))
        ]        
        [ G.Edge 1 2 NoLabel
        ]

type EdgeLabel = NoLabel

expandfnode : Int -> MSGraph -> MSGraph
expandfnode id g =
    let nctx = G.get id g
    in
        case nctx of

            Just nc ->
                case (Debug.log "l:" nc.node.label) of
                    FuncNode l (MS mf) -> 
                        g
                          |> G.remove id
                          |> G.insert
                              { nc
                              | node = G.Node nc.node.id <| FuncNode "Split" <| Split (F.prim split)
                              }
                          |> insertLeftSplit nc.node.id
                          |> insertRightSplit nc.node.id
                    
                    FuncNode l (Merge mf) -> g

                    _ -> g
 
            Nothing -> g


nextId : MSGraph -> Int
nextId g =
    case G.nodeIdRange g of
        Just (mni, mxi) -> mxi + 1
        Nothing -> 1


insertLeftSplit : Int -> MSGraph -> MSGraph
insertLeftSplit pid g =
    g 
    |> G.insert
        { node = G.Node (nextId g) (DataNode <| ms [40])
        , incoming = IntDict.singleton pid NoLabel
        , outgoing = IntDict.empty
        }


insertRightSplit : Int -> MSGraph -> MSGraph
insertRightSplit pid g =
    g 
    |> G.insert
        { node = G.Node (nextId g) (DataNode [30, 40])
        , incoming = IntDict.singleton pid NoLabel
        , outgoing = IntDict.empty
        }

type alias MSGraph 
    = Graph 
        Node
        EdgeLabel
