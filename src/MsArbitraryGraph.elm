module MsArbitraryGraph exposing (..)

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




            
{--

pt = root|l|r
p=(pt)+[/s]

merge_a_pt
merge_n_pt

ms_n_pt
ms_a_pt

split_n_pt
split_a_pt

--}            



            

viewSplitBar : Int -> Int -> Svg msg
viewSplitBar index nitems = Svg.rect [][]
    
            

pointToString : Point -> String
pointToString pt =
    String.concat [(String.fromFloat pt.x), ",", (String.fromFloat pt.y)]
        

pointsToString : List Point -> String
pointsToString pts =
    String.join " " (List.map pointToString pts)

        

getData : String -> List GDataNode -> List Int
getData p nl =
    case (LE.find (\n -> n.v == p) nl) of
        Just n -> n.value.label |> labelToData
        _ -> []                   

             
split_end : List Int -> Int -> String -> List GDataNode -> List GDataEdge -> (List GDataNode, List GDataEdge)
split_end toSplit splitIdx path nodes edges =
    ( List.append
          [ { v = (path++"-l")
            , value = (nodeFromLabelVal
                           (List.take splitIdx toSplit))
            }
          , { v = (path++"-r")
            , value = (nodeFromLabelVal
                           (List.drop splitIdx toSplit))
            }
          ]
          ( List.map
                (\n ->
                     if n.v == "split_n_"++path then
                         { n | v = "split_a_"++path }
                     else
                         n
                )
                nodes
          )
    , List.append                
          [ { v = "split_a_"++path
            , w = path++"-l"
            , value = defEdVal
            }
          , { v = "split_a_"++path
            , w = path++"-r"
            , value = defEdVal
            }
          ]
          ( edges
          |> List.map
                (\e ->
                     if e.v == "split_n_"++path then
                         { e | v = "split_a_"++path }
                     else                               
                         if e.w == "split_n_"++path then
                             { e | w = "split_a_"++path }
                         else
                             e
                )
          )
    )

   
    
updateSorted : GDataNode -> GDataNode
updateSorted n =
    if String.startsWith "$" n.v && issorted (labelToData n.value.label) then
        { n | v = n.v ++ "/s" }
    else
        n
    

split : String -> List GDataNode -> List GDataEdge -> (List GDataNode, List GDataEdge)
split path nodes edges =
    let
        toSplit =
            case (LE.find (\n -> n.v == path) nodes) of
                Just n -> n.value.label |> labelToData
                _ -> []
                               
        dlen = List.length (toSplit)
                                      
        splitIdx = ceiling <| toFloat dlen / 2
                   
        leftval = List.take splitIdx toSplit
        rightval = List.drop splitIdx toSplit
    in
        if dlen <= 1 then
            (nodes, edges)
        else
            if dlen == 2 then
                split_end toSplit splitIdx path nodes edges
            else
                ( List.concat
                      [ (if List.length leftval == 1 then
                             []
                         else
                             [ { v = "split_n_" ++ path ++ "-l"
                               , value = { defNodeVal | label = "Split" }
                               }
                             ])
                      , (if List.length rightval == 1 then
                             []
                         else
                             [ { v = "split_n_" ++ path ++ "-r"
                               , value = { defNodeVal | label = "Split" }
                               }
                             ])
                      , [ { v = path++"-l"
                          , value = (nodeFromLabelVal leftval)
                          }
                        , { v = path++"-r"
                          , value = (nodeFromLabelVal rightval)
                          }
                        ]
                      , ( List.map
                              (\n ->
                                   if n.v == "split_n_"++path then
                                       { n | v = "split_a_"++path }
                                   else
                                       n
                              )
                              nodes
                        )
                      ] --|> List.map updateSorted
                      
                , List.concat
                      [[ { v = "split_a_"++path
                         , w = path++"-l"
                         , value = defEdVal
                         }
                       , { v = "split_a_"++path
                         , w = path++"-r"
                         , value = defEdVal
                         }
                       ]
                      , (if List.length leftval == 1 then
                             []
                        else                            
                            [{ v = path ++ "-l"
                             , w = "split_n_" ++ path ++ "-l"
                             , value = defEdVal
                             }
                            ])
                      , (if List.length rightval == 1 then
                             []
                         else                            
                             [{ v = path ++ "-r"
                              , w = "split_n_" ++ path ++ "-r"
                              , value = defEdVal
                              }
                             ])                      
                      , ( edges
                        |> List.map
                              (\e ->
                                   if e.v == "split_n_"++path then
                                       { e | v = "split_a_"++path }
                                   else                               
                                       if e.w == "split_n_"++path then
                                           { e | w = "split_a_"++path }
                                       else
                                           e
                              )
                        )
                      ]
                )
                


mergeArbitrary: String -> String -> List GDataNode -> List GDataEdge -> (List GDataNode, List GDataEdge)
mergeArbitrary p1 p2 nl el =
    let mnv = nl
            |> List.filter (\n -> List.member n.v [p1, p2])
            |> List.map (\n -> n.v)
            |> String.join "+"
    in
        ( List.append
              (nl |> List.map
                   ((\n -> if ("merge_n_" ++ p1 ++ "_" ++ p2) == n.v then
                              { n | v = ("merge_a_" ++ p1 ++ "_" ++ p2) }
                          else
                              if ("merge_n_" ++ p2 ++ "_" ++ p1) == n.v then
                                  {n | v = ("merge_a_" ++ p2 ++ "_" ++ p1) }
                              else
                                  n
                   )
                   >>
                   (\n ->
                        if n.v == p1 then
                            {n | v = p1 ++ "/m"}
                        else
                            if n.v == p2 then
                                {n | v = p2 ++ "/m"}
                            else
                                n
                   ))
              )
              [ { v = mnv
                , value = nodeFromLabelVal
                          <| M.merge
                      (getData (p1) nl)
                      (getData (p2) nl)
                }
              ]
        , List.append (el |> List.map
                           ((\e -> if ("merge_n_" ++ p1 ++ "_" ++ p2) == e.w then
                                      {e | w = ("merge_a_" ++ p1 ++ "_" ++ p2)}
                                  else
                                      if ("merge_n_" ++ p2 ++ "_" ++ p1) == e.w then
                                          {e | w = ("merge_a_" ++ p2 ++ "_" ++ p1)}
                                      else
                                          e)
                                >>
                           (\e ->
                              if e.v == p1 then
                                  {e | v = p1 ++ "/m"}
                              else
                                  if e.v == p2 then
                                      {e | v = p2 ++ "/m"}
                                  else
                                      e
                                  )
                                >>
                           (\e ->
                              if e.w == p1 then
                                  {e | w = p1 ++ "/m"}
                              else
                                  if e.w == p2 then
                                      {e | w = p2 ++ "/m"}
                                  else
                                      e
                                  ))

                      )
            [ { v = if List.member ("merge_n_" ++ p1 ++ "_" ++ p2) (List.map (\n-> n.v) nl) then
                        ("merge_a_" ++ p1 ++ "_" ++ p2)
                    else
                        ("merge_a_" ++ p2 ++ "_" ++ p1)
              , w = mnv
              , value = defEdVal
              }                  
            ]
        )
    
{--

1->2
1->3

1->2
1->3

pick a set of edges


--}
issorted : List Int -> Bool
issorted l1 =
    List.all (\x -> x == True) <| List.map2 (\i1 i2 -> i1 == i2) l1 (List.sort l1)
