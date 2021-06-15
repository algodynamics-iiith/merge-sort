module V1.LayoutGraph exposing ( fromNodesAndEdges, node, edge
                               , LayoutGraph, Node, Edge, lgEncode
                               , lgDecoder )

import Json.Decode as D exposing (Decoder, Value)
import Json.Encode as JE
import List.Extra as LE


type alias Options =
    { directed : Bool
    , multigraph : Bool
    , compound : Bool
    }

optionsDecoder : Decoder Options
optionsDecoder =
    D.map3 Options
        (D.field "directed" D.bool)
        (D.field "multigraph" D.bool)
        (D.field "compound" D.bool)
            

optionsEncode : Options -> Value
optionsEncode o =
    JE.object
        [ ("directed", JE.bool o.directed)
        , ("multigraph", JE.bool o.multigraph)
        , ("compound", JE.bool o.compound)
        ]

type alias NodeValue =
    { label : String
    , width : Float
    , height : Float
    , x : Float
    , y : Float
    }

nvDecoder : Decoder NodeValue
nvDecoder =
    D.map5 NodeValue
        (D.field "label" D.string)
        (D.field "width" D.float)
        (D.field "height" D.float)
        (D.field "x" D.float)
        (D.field "y" D.float)            


nvEncode : NodeValue -> Value
nvEncode nv =
    JE.object
        [ ("label", JE.string nv.label)
        , ("width", JE.float nv.width)
        , ("height", JE.float nv.height)
        , ("x", JE.float nv.x)
        , ("y", JE.float nv.y)
        ]

type alias Node =
    { v : String
    , value : NodeValue
    }


nodeDecoder : Decoder Node
nodeDecoder =
    D.map2 Node
        (D.field "v" D.string)
        (D.field "value" nvDecoder)


nodeEncode : Node -> Value
nodeEncode n =
    let {value} = n
    in
        JE.object
            [ ("v", JE.string n.v)
            , ("value", nvEncode value)
            ]

node : String -> String -> Node
node v l =
    let 
        nv = 
            { width = 20
            , height = 20
            , x = 0
            , y = 0
            , label = l
            }
    in
        { v = v
        , value = nv
        }

type alias Point =
    { x : Float
    , y : Float
    }

pointDecoder : Decoder Point
pointDecoder =
    D.map2 Point
        (D.field "x" D.float)
        (D.field "y" D.float)


pointEncode : Point -> Value
pointEncode p =
    JE.object
        [ ("x", JE.float p.x)
        , ("y", JE.float p.y)
        ]

type alias EdgeValue =
    { label : String
    , points : List Point
    }

evDecoder : Decoder EdgeValue
evDecoder =
    D.map2 EdgeValue
        (D.field "label" D.string)
        (D.field "points" (D.list pointDecoder))


evEncode : EdgeValue -> Value
evEncode ev =
    JE.object
        [ ("label", JE.string ev.label)
        , ("points", JE.list pointEncode ev.points)
        ]

type alias Edge =
    { v : String
    , w : String
    , value : EdgeValue
    }

edgeDecoder : Decoder Edge
edgeDecoder =
    D.map3 Edge
        (D.field "v" D.string)
        (D.field "w" D.string)
        (D.field "value" evDecoder)
            

edgeEncode : Edge -> Value
edgeEncode e =
    let {value} = e
    in
        JE.object
            [ ("v", JE.string e.v)
            , ("w", JE.string e.w)
            , ("value", evEncode value)
            ]

edge : String -> String -> Edge
edge from to =
    { v = from
    , w = to
    , value = 
        { label = ""
        , points = []
        }
    }

type alias LayoutGraph =
    { options: Options
    , nodes : List Node
    , edges : List Edge
    , value : Dims
    }

type alias Dims = { width: Float, height: Float }

gdimDecoder : Decoder Dims    
gdimDecoder =
    D.map2 Dims
        (D.field "height" D.float)
        (D.field "width" D.float)
            
    
lgDecoder : Decoder LayoutGraph
lgDecoder =
    D.map4 LayoutGraph
        (D.field "options" optionsDecoder)
        (D.field "nodes" (D.list nodeDecoder))
        (D.field "edges" (D.list edgeDecoder))
        (D.field "value" gdimDecoder)


lgEncode : LayoutGraph -> Value
lgEncode lg =
    JE.object
        [ ("options", optionsEncode lg.options)
        , ("nodes", (JE.list nodeEncode) lg.nodes)
        , ("edges", (JE.list edgeEncode) lg.edges)
        , ("value", (JE.object [("height", JE.float 0), ("width", JE.float 0)]))
        ]

defaultOptions : Options
defaultOptions =
    { directed = True
    , multigraph = False
    , compound = False
    }

defaultGraphSize : Dims
defaultGraphSize = Dims 0 0

fromNodesAndEdges : List Node -> List Edge -> LayoutGraph
fromNodesAndEdges ns es =
    { options = defaultOptions
    , nodes = ns
    , edges = es
    , value = defaultGraphSize
    }
