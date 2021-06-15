module DG3 exposing (..)

import Json.Decode as D exposing (Decoder, Value)
import Json.Encode as JE
import List.Extra as LE
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Config

import LMerge as M
import NodeId as NI

type alias GDataOptions =
    { directed : Bool
    , multigraph : Bool
    , compound : Bool
    }
   
type Msg = Expand String | Merge String

    
update : Msg -> GData -> GData
update msg graph =
    case msg of
        Expand np ->
            let (n_, e_) = expand np graph.nodes graph.edges
            in
                { graph
                    | nodes = List.sortWith (\m n -> NI.compIds m.v n.v) n_
                    , edges = List.sortWith (\m n -> NI.compIds m.v n.v) e_
                }

        Merge np ->
            let (n_, e_) = merge np graph.nodes graph.edges
            in
                { graph
                    | nodes = List.sortWith (\m n -> NI.compIds m.v n.v) n_
                    , edges = List.sortWith (\m n -> NI.compIds m.v n.v) e_
                }
        
    
gdOptionDecoder : Decoder GDataOptions
gdOptionDecoder =
    D.map3 GDataOptions
        (D.field "directed" D.bool)
        (D.field "multigraph" D.bool)
        (D.field "compound" D.bool)
            

gdOptionEncode : GDataOptions -> Value
gdOptionEncode gdo =
    JE.object
        [ ("directed", JE.bool gdo.directed)
        , ("multigraph", JE.bool gdo.multigraph)
        , ("compound", JE.bool gdo.compound)
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

            
type alias GDataNode =
    { v : String
    , value : NodeValue
    }



gdNodeDecoder : Decoder GDataNode
gdNodeDecoder =
    D.map2 GDataNode
        (D.field "v" D.string)
        (D.field "value" nvDecoder)


gdNodeEncode : GDataNode -> Value
gdNodeEncode gdn =
    let {value} = gdn
    in
        JE.object
            [ ("v", JE.string gdn.v)
            , ("value", nvEncode value)
            ]


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
        
        
type alias GDataEdge =
    { v : String
    , w : String
    , value : EdgeValue
    }    

gdEdgeDecoder : Decoder GDataEdge
gdEdgeDecoder =
    D.map3 GDataEdge
        (D.field "v" D.string)
        (D.field "w" D.string)
        (D.field "value" evDecoder)
            

gdEdgeEncode : GDataEdge -> Value
gdEdgeEncode gde =
    let {value} = gde
    in
        JE.object
            [ ("v", JE.string gde.v)
            , ("w", JE.string gde.w)
            , ("value", evEncode value)
            ]

    
type alias GData =
    { options: GDataOptions
    , nodes : List GDataNode
    , edges : List GDataEdge
    , value : Dims
    }

type alias Dims = { width: Float, height: Float }

gdimDecoder : Decoder Dims    
gdimDecoder =
    D.map2 Dims
        (D.field "height" D.float)
        (D.field "width" D.float)
            
    
gdataDecoder : Decoder GData
gdataDecoder =
    D.map4 GData
        (D.field "options" gdOptionDecoder)
        (D.field "nodes" (D.list gdNodeDecoder))
        (D.field "edges" (D.list gdEdgeDecoder))
        (D.field "value" gdimDecoder)


            
gdEncode : GData -> Value
gdEncode gd =
    JE.object
        [ ("options", gdOptionEncode gd.options)
        , ("nodes", (JE.list gdNodeEncode) gd.nodes)
        , ("edges", (JE.list gdEdgeEncode) gd.edges)
        , ("value", (JE.object [("ranker", JE.string "network-simplex")]))
        ]



-- find a node with v in a given graph
getNode : GData -> String -> Maybe GDataNode
getNode graph v =
    graph.nodes |> LE.find (\n -> n.v == v)

defNodeVal =
    { label = ""
    , width = Config.opNodeWidth
    , height = Config.opNodeHeight
    , x = 0
    , y = 0
    }


nodeFromLabelVal : List Int -> NodeValue
nodeFromLabelVal lst =
    { label = lst |> List.map String.fromInt |> String.join " "
    , width = Config.node_width (List.length lst)
    , height = Config.node_height
    , x = 0
    , y = 0
    }


labelToData : String -> List Int
labelToData slst =
    slst
        |> String.split " "
        |> List.map
           (\lv -> String.toInt lv
           |> Maybe.withDefault -1
           )
           
    
defEdVal =
    { label = ""
    , points = [Point 0 0]
    }



viewDataNode : GDataNode -> Svg Msg
viewDataNode node =
    let {value} = node

        h = value.height |> String.fromFloat
        w = value.width |> String.fromFloat

        bx = value.x - value.width/2 |> String.fromFloat
        by = value.y - value.height/2 |> String.fromFloat

        tx = (value.x - value.width / 2 + Config.nodeCfg.padding) |> String.fromFloat
        ty = (value.y) |> String.fromFloat
        
        box =
            Svg.rect
                [ SA.height h
                , SA.width w
                , SA.stroke "#111"
                , SA.strokeWidth "1"
                , SA.fill "#fff"
                , SA.x bx
                , SA.y by
                ]
                []

        txt = Svg.text_
                [ SA.height h
                , SA.width w
                , SA.x tx
                , SA.y ty
                , SA.fill <| if String.endsWith "/s" node.v then "green" else "black"
                , SA.fontSize (String.fromInt 50)
                , SA.alignmentBaseline "middle"
                ]
                [ Svg.text value.label
                ]
              
        pt =
            Svg.circle
                [ SA.cx (String.fromFloat value.x)
                , SA.cy (String.fromFloat value.y)
                , SA.r (String.fromFloat 10)
                ]
                []
    in
        Svg.g
            []
            [ --box
            txt
            ]


            
viewOpNode : GDataNode -> Svg Msg
viewOpNode node =
    let {value} = node

        h = value.height |> String.fromFloat
        w = value.width |> String.fromFloat

        bx = value.x - value.width/2 |> String.fromFloat
        by = value.y - value.height/2 |> String.fromFloat

        tx = (value.x - value.width / 2 + Config.nodeCfg.padding) |> String.fromFloat
        ty = (value.y) |> String.fromFloat
        
        box =
            Svg.rect
                [ SA.height h
                , SA.width w
                , SA.stroke "#111"
                , SA.strokeWidth "1"
                , SA.fill "#E3F2FD"
                , SA.x bx
                , SA.y by
                ]
                []

        txt = Svg.text_
                [ SA.height h
                , SA.width w
                , SA.x tx
                , SA.y ty
                , SA.fontSize (String.fromInt 50)
                , SA.alignmentBaseline "middle"
                ]
                [ Svg.text value.label
                ]

        attrs = if String.contains "_n_" node.v then
                    if String.startsWith "ms_n_" node.v then
                        [ SE.onClick (Expand (String.dropLeft 5 node.v))
                        , SA.cursor "pointer"
                        ]
                    else
                        if String.startsWith "merge_n_" node.v then
                        [ SE.onClick (Merge (String.dropLeft 8 node.v))
                        , SA.cursor "pointer"
                        ]
                        else
                            []
                else
                    []
    in
        Svg.g
            attrs
            (if String.contains "_n_" node.v then 
                 [ box, txt ]
             else 
                 [ txt ]
            )
            
{--

pt = root|l|r
p=(pt)+[/s]

merge_a_pt

merge_n_pt

ms_n_pt

ms_a_pt

split_pt

--}            



viewNode : GDataNode -> Svg Msg
viewNode node =    
    let v = node.v
        vl = String.split "_" v
    in
        case vl of
            kw :: [] -> viewDataNode node
            lw :: rest  -> viewOpNode node
            [] -> Svg.rect[][]
            

viewSplitBar : Int -> Int -> Svg Msg
viewSplitBar index nitems = Svg.rect [][]
    
            

pointToString : Point -> String
pointToString pt =
    String.concat [(String.fromFloat pt.x), ",", (String.fromFloat pt.y)]
        

pointsToString : List Point -> String
pointsToString pts =
    String.join " " (List.map pointToString pts)

        
viewEdge : GDataEdge -> Svg Msg
viewEdge edge =
    let {value} = edge
        end = LE.last value.points |> Maybe.withDefault (Point 0 0)
        pre_end = LE.getAt ((List.length value.points) - 1) value.points
                |> Maybe.withDefault (Point 0 0)
        
    in
        Svg.g
            []
            [ Svg.polyline
                  [ SA.stroke "#9E9E9E"
                  , SA.strokeWidth "2"
                  , SA.fill "none"
                  , SA.points (pointsToString value.points)
                  ]
                  []
            , Svg.polygon
                  [ SA.stroke "#9E9E9E"
                  , SA.strokeWidth "2"
                  , SA.fill "#9E9E9E"
                  , SA.points
                        <| pointsToString
                            [ end
                            , Point (end.x + 5) (end.y - 10)
                            , Point (end.x - 5) (end.y - 10)
                            , Point (end.x) (end.y)
                            ]
                  ]
                  []
            ]       


mergeroot : List GDataNode -> List GDataEdge -> (List GDataNode, List GDataEdge)
mergeroot nodes edges =
    let sorted_data = M.merge
                      (getData ("$-l/s") nodes)
                      (getData ("$-r/s") nodes)
                          
        sorted_node = { v = "$/s"
                      , value = nodeFromLabelVal sorted_data
                      }

        last_edge = { v = "merge_a_$"
                    , w = "$/s"
                    , value = defEdVal
                    }
    in
        ( nodes
        |> List.map (\n -> if n.v == "merge_n_$" then
                               {n | v = "merge_a_$"}
                           else n)
        |> List.append [ sorted_node ]
        , edges
        |> List.map (\e -> if e.v == "merge_n_$" then
                               {e | v = "merge_a_$"}
                           else
                               if e.w == "merge_n_$" then
                                   {e | w = "merge_a_$"}
                               else e)

        |> List.append [ last_edge ]
        )

            
merge : String -> List GDataNode -> List GDataEdge -> (List GDataNode, List GDataEdge)
merge path nodes edges =
    let incoming_data = List.filter (\n -> (n.v == path++"-l/s") || (n.v == path++"-r/s")) nodes
    in
        if List.length incoming_data < 2 then
            (nodes, edges)
        else
            if path == "$" then
                mergeroot nodes edges
            else
                let medges =
                        ( { v = "merge_n_"++path
                          , w = path++"/s"
                          , value = defEdVal
                          }
                        ) :: [ { v = path++"/s"
                               , w = ("merge_n_"++(String.dropRight 2 path))
                               , value = defEdVal
                               }
                             ]
                in
                    
                    ( nodes
                    |> List.map (\n -> if n.v == "merge_n_"++path then
                                           {n | v = "merge_a_"++path}
                                       else n
                                )
                    |> List.append
                          [ { v = path++"/s"
                            , value = nodeFromLabelVal
                                      <| M.merge
                                  (getData (path++"-l/s") nodes)
                                  (getData (path++"-r/s") nodes)
                            }
                          ]
                    , edges
                    |> List.filter (\e -> not ((e.v == "merge_n_"++path) && (e.w == "merge_n_"++(String.dropRight 2 path))))
                    |> List.append medges
                    |> List.map (\n -> if n.v == "merge_n_"++path then
                                           {n | v = "merge_a_"++path}
                                       else
                                           if n.w == "merge_n_"++path then
                                               {n | w = "merge_a_"++path}
                               else n
                                )
                    )

    {--
graph
mst

design
implementation
effectiveness

3 workshops


    --}

getData : String -> List GDataNode -> List Int
getData p nl =
    case (LE.find (\n -> n.v == p) nl) of
        Just n -> n.value.label |> labelToData
        _ -> []
                       
    
expand : String -> List GDataNode -> List GDataEdge -> (List GDataNode, List GDataEdge)
expand path nodes edges =
    let
        toSplit =
            case (LE.find (\n -> n.v == path) nodes) of
                Just n -> n.value.label |> labelToData
                _ -> []
                
        dlen = List.length (toSplit)
    in
        if dlen > 1 then
            expandgt2 path nodes edges
        else
            let fullExpendGraph = expandSingleton path nodes edges
            in
                fullExpendGraph


                    
expandgt2 : String -> List GDataNode -> List GDataEdge -> (List GDataNode, List GDataEdge)
expandgt2 path nodes edges =
    let
        toSplit =
            case (LE.find (\n -> n.v == path) nodes) of
                Just n -> n.value.label |> labelToData
                _ -> []
                
        dlen = List.length (toSplit)

        splitIdx = ceiling <| toFloat dlen / 2
                   
        exp_nodes =
            [ { v = ("split_a_"++path)
              , value = { defNodeVal
                        | label = "split"
                        }
              }
            , { v= (path++"-l")
              , value = (nodeFromLabelVal
                             (List.take splitIdx toSplit))
              }
            , { v= (path++"-r")
              , value = (nodeFromLabelVal
                             (List.drop splitIdx toSplit))
              }
            , { v= ("ms_n_"++path++"-l")
              , value = { defNodeVal
                            | label = "ms"
                        }
              }
            , { v= ("ms_n_"++path++"-r")
              , value = { defNodeVal
                            | label = "ms"
                        }
              }
            , { v= ("merge_n_"++path)
              , value = { defNodeVal
                            | label = "merge"
                        }
              }
            ]
            
        exp_edges =
            [ { v = path
              , w = "split_a_"++path
              , value = defEdVal
              }
            , { v = "split_a_"++path
              , w = path++"-l"
              , value = defEdVal
              }
            , { v = "split_a_"++path
              , w = path++"-r"
              , value = defEdVal
              }
            , { v = path++"-l"
              , w = ("ms_n_"++path++"-l")
              , value = defEdVal
              }
            , { v = path++"-r"
              , w = ("ms_n_"++path++"-r")
              , value = defEdVal
              }
            , { v = ("ms_n_"++path++"-l")
              , w = ("merge_n_"++path)
              , value = defEdVal
              }
            , { v = ("ms_n_"++path++"-r")
              , w = ("merge_n_"++path)
              , value = defEdVal
              }
            ]

    in
        ( List.append (nodes |> List.filter (\n -> not (n.v == "ms_n_"++path))) exp_nodes
        , List.append
              ( edges
              |> List.map (\e -> if (e.v == "ms_n_"++path) then {e | v = "merge_n_"++path} else e)
              |> List.filter (\e -> not <| List.member ("ms_n_"++path) [e.v, e.w])
              ) exp_edges
        )


expandSingleton : String -> List GDataNode -> List GDataEdge -> (List GDataNode, List GDataEdge)
expandSingleton path nodes edges =
    let
        toSplit =
            case (LE.find (\n -> n.v == path) nodes) of
                Just n -> n.value.label |> labelToData
                _ -> []               
                   
        exp_nodes =
            [ { v = (path++"/s")
              , value = toSplit
              |> List.sort
              |> nodeFromLabelVal
              }
            , { v= ("ms_a_"++path)
              , value = {defNodeVal | label = "ms"}
              }
            ]
            
        exp_edges =
            [ { v = path
              , w = "ms_a_"++path
              , value = defEdVal
              }
            , { v = "ms_a_"++path
              , w = (path++"/s")
              , value = defEdVal
              }
            , { v = path++"/s"
              , w = "merge_n_"++(String.dropRight 2 path)
              , value = defEdVal
              }
            ]
    in
        ( List.append (nodes |> List.filter (\n -> not <| n.v == "ms_n_"++path)) exp_nodes
        , List.append (edges |> List.filter (\e -> not <| ((e.v == "ms_n_"++path) || (e.w == "ms_n_"++path)))) exp_edges
        )
