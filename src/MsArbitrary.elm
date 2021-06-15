port module MsArbitrary exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Json.Decode as D exposing (Decoder, Value)
import Json.Encode as JE
import List.Extra as LE
import Array exposing (Array)
import Random
import Random.Array as RA
import Config
import MsArbitraryGraph as MAG exposing (..)


import LMerge as M
import NodeId as NI
import Prompt as P


-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




-- PORTS


port sendMessage : Value -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg
    

        
type alias Model =
    { graph: GData
    , selectedNodes: (Maybe String, Maybe String)
    , data: List Int
    , prompt: P.Prompt
    }
                       

defaultGDOptions =
    { directed = True
    , multigraph = False
    , compound = False
    }

-- 24 => 26
-- 

ig1 data =
    { options = defaultGDOptions
    , nodes =
          [ { v = "$"
            , value = nodeFromLabelVal data
            }
          , { v = "split_n_$"
            , value = { defNodeVal | label = "Split" }
            }
          ]
    , edges =
          [ { v = "$"
            , w = "split_n_$"
            , value = defEdVal
            }
          ]
    , value = {width = 0, height = 0}
    }


    
init : () -> ( Model, Cmd Msg )
init flags =
  ( { graph = ig1 []
    , selectedNodes = (Nothing, Nothing)
    , data = []
    , prompt = ("",P.PromptInfo)
    }
  , Cmd.batch
        [ Random.generate
            GotRandomArray
            (RA.rangeLengthArray 4 6
                 (Random.int 10 50))
        ]
  )


-- UPDATE


type Msg
  = Send
  | Recv String
  | GotRandomArray (Array Int)
  | Expand String
  | Merge String
  | Split String
  | SelectNode String


subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver Recv


      
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    Send ->
      ( model
      , sendMessage (gdEncode model.graph)
      )

    Recv message ->
        let res = 
                D.decodeString gdataDecoder message
        in
            case res of
                Ok gdata -> 
                    ( { model
                          | graph = gdata
                      }
                    , Cmd.none                      
                    )
                Err e -> (model, Cmd.none)
            
    GotRandomArray arr ->
        let gr_ = ig1 <| Array.toList arr
        in
            ( { model
                  | graph = gr_
                  , data = Array.toList arr
                  , prompt = ("Click on the 'Split' node to split the given list.",P.PromptInfo)
              }
            , sendMessage (gdEncode gr_)
            )

    Split np ->
        let {graph} = model
            (n_, e_) = split np graph.nodes graph.edges
            gr_ = { graph
                      | nodes = List.sortWith (\m n -> NI.compIds m.v n.v) n_
                      , edges = List.sortWith (\m n -> NI.compIds m.v n.v) e_
                  }
        in
            ( { model
                  | graph = gr_
                  , prompt = ("Click on any 'Split' node to split the corresponding sublist.",P.PromptInfo)
              }
            , sendMessage (gdEncode gr_)
            )
        
    Merge np ->
        let {graph, selectedNodes} = model            
            (n_, e_) = mergeArbitrary
                       ( Maybe.withDefault "" <| Tuple.first selectedNodes )
                       ( Maybe.withDefault "" <| Tuple.second selectedNodes )
                       graph.nodes graph.edges
            gr_ = { graph
                      | nodes = List.sortWith (\m n -> NI.compIds m.v n.v) n_
                      , edges = List.sortWith (\m n -> NI.compIds m.v n.v) e_
                  }
        in
            ( { model
                  | graph = gr_
                  , selectedNodes = (Nothing, Nothing)
                  , prompt = if ( 2 == ((List.length <|
                                         List.filter
                                             (\n -> (labelToData n.value.label |> List.length) == (List.length model.data))
                                             gr_.nodes) |> Debug.log "l")
                                   ) then
                                    ("Mergesort Complete.",P.PromptSuccess)
                                else
                                    ("Click on two sublists to merge them",P.PromptInfo)                    
              }
            , sendMessage (gdEncode gr_)
            )
            
    SelectNode p ->
        case model.selectedNodes of
            
            (Nothing, Nothing) ->
                ( { model
                      | selectedNodes = (Just p, Nothing)
                      , prompt = ("Click on another sublist to merge the two selected sublists.",P.PromptInfo)
                  }
                , Cmd.none
                )
                
            (Just a, Nothing) ->
                
                if a == p then
                    ( { model
                          | selectedNodes = (Nothing, Nothing)
                          , graph = model.graph
                          , prompt = ("",P.PromptInfo)
                      }
                    , Cmd.none
                    )
                    
                else
                    let gr_ = removeSplitNode a p <| addMergeNode a p <| model.graph
                    in
                        ( { model
                              | selectedNodes = (Just a, Just p)
                              , graph = gr_
                              , prompt = ("Click on 'Merge' to merge the selected sublists.",P.PromptInfo)
                          }
                        , sendMessage (gdEncode gr_)
                        )
                    
                
            (Just a, Just b) ->
                ( { model
                      | prompt = ("merge the selected sublists and then select other sublists to merge.",P.PromptInfo)
                  }
                      {--
                 { model
                      | selectedNodes = (if p == a then
                                             (Just b, Nothing)
                                         else
                                             if p == b then
                                                 (Just a, Nothing)
                                             else
                                                 (Just a, Just b)
                                        )
                      , graph = (if p == a || p == b then
                                     addSplitNodes a b model.graph |> removeMergeNode a b
                                 else
                                     model.graph
                                )
                                   
                  }--}
                , Cmd.none
                )
                                
            _ ->
                ( model          
                , Cmd.none
                )
                
    _ -> (model, Cmd.none)
                    
            

view : Model -> Html Msg
view model =
    let {graph} = model
    in
        Html.div
            [ HA.class "flex-grow w-2/3 flex flex-col"
            ]
            [ P.show model.prompt
            , Html.div
                  [ HA.class "flex flex-grow justify-center overflow-scroll"
                  , HA.attribute "data-zoom-on-wheel" ""
                  ,  HA.attribute "data-pan-on-drag" ""
                  ]
                  [ viewGraph model ]
            ]            


viewGraph : Model -> Html Msg
viewGraph {graph, selectedNodes, data} =
    let vh = max 800 (graph.value.height * 1.1)
        vw = max 800 (graph.value.width * 1.1)
    in
        Svg.svg
            [ SA.viewBox <|
                  "0 0 "
                  ++ (String.fromFloat <| vh)
                  ++ " "
                  ++ (String.fromFloat <| vw)
                  
            , SA.width "700px"
            , SA.height "500px"
            , HA.style "height" "100%"
            ]
            [ Svg.g [] (List.map (\n -> viewNode n selectedNodes data) graph.nodes)
            , Svg.g [] (List.map viewEdge graph.edges)
            ]                        


viewControls: Html Msg
viewControls =
    Html.div
        []
        [ Html.text "MERGE"
        ]



viewEdge : GDataEdge -> Svg Msg
viewEdge edge =
    let {value} = edge
        end = LE.last value.points |> Maybe.withDefault (Point 0 0)
        start = List.head value.points
              |> Maybe.withDefault (Point 0 0)
        
    in
        Svg.g
            []
            [ Svg.polyline
                  [ SA.stroke "#9E9E9E"
                  , SA.strokeWidth "2"
                  , SA.fill "none"
                  , SA.points (pointsToString [start, {end | y = end.y - 10}]) --value.points)
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
        
viewNode : GDataNode -> (Maybe String, Maybe String) -> List Int -> Svg Msg
viewNode node selectedNodes data =
    let v = node.v
        vl = String.split "_" v
    in
        case vl of
            kw :: [] -> viewDataNode node selectedNodes data
            lw :: rest  -> viewOpNode node
            [] -> Svg.rect[][]

            
viewOpNode : GDataNode -> Svg Msg
viewOpNode node =
    let {value} = node

        h = value.height |> String.fromFloat
        w = value.width |> String.fromFloat

        bx = value.x - value.width/2 |> String.fromFloat
        by = value.y - value.height/2 |> String.fromFloat

        --tx = (value.x - value.width / 2 + Config.nodeCfg.padding) |> String.fromFloat
        tx = value.x |> String.fromFloat
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
                , SA.fontSize (String.fromInt 30)
                , SA.alignmentBaseline "middle"
                , SA.textAnchor "middle"
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
                            if String.startsWith "split_n_" node.v then
                                [ SE.onClick (Split (String.dropLeft 8 node.v))
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



viewDataNode : GDataNode -> (Maybe String, Maybe String) -> List Int -> Svg Msg
viewDataNode node selectedNodes data =
    let {value} = node

        h = value.height |> String.fromFloat
        w = value.width |> String.fromFloat

        bx = value.x - value.width/2 |> String.fromFloat
        by = value.y - value.height/2 |> String.fromFloat

        tx = (value.x - value.width / 2 + Config.nodeCfg.padding) |> String.fromFloat
        ty = (value.y) |> String.fromFloat

        boxCol =
            (case selectedNodes of
                 (Just a, Just b) -> if List.member node.v [a, b] then "#FFEB3B" else "#fff"
                 (Just a, Nothing) -> if List.member node.v [a] then "#FFEB3B" else "#fff"
                 _ -> "#fff"
            )
        
        box =
            Svg.rect
                [ SA.height h
                , SA.width w
                , SA.stroke "#111"
                , SA.strokeWidth "1"
                , SA.fill boxCol
                , SA.x bx
                , SA.y by
                ]
                []

        txt = Svg.text_
                [ SA.height h
                , SA.width w
                , SA.x tx
                , SA.y ty
                , SA.fill <| if (String.contains "+" node.v) || ((labelToData value.label |> List.length) == 1) then "green" else "black"
                , SA.fontSize (String.fromInt 50)
                , SA.alignmentBaseline "middle"
                ]
                [ Svg.text <| value.label
                ]
              
        pt =
            Svg.circle
                [ SA.cx (String.fromFloat value.x)
                , SA.cy (String.fromFloat value.y)
                , SA.r (String.fromFloat 10)
                ]
                []
    in
        if (((labelToData value.label |> List.length) == 1) || (String.contains "+" node.v))
            && (not <| String.endsWith "/m" node.v)
            && (not <| ((labelToData value.label |> List.length) == (List.length data)))
        then
            Svg.g
                [ SE.onClick (SelectNode node.v)
                , SA.cursor "pointer"
                ]
                [ box
                , txt
                ]
        else
            Svg.g
                []
                [ txt ]


addMergeNode : String -> String -> GData -> GData
addMergeNode a p graph =
    if List.member ("merge_n_" ++ a ++ "_"++ p) (List.map .v graph.nodes) || List.member ("merge_n_" ++ p ++ "_"++ a) (List.map .v graph.nodes) then
        graph
    else
        let n_ = List.append graph.nodes
                 [ { v = ("merge_n_" ++ a ++ "_"++ p)
                   , value = { defNodeVal | label = "Merge" }
                   }
                 ]
            e_ = List.append graph.edges
                 [ { v = a
                   , w = ("merge_n_" ++ a ++ "_"++ p)
                   , value = defEdVal
                   }
                 , { v = p
                   , w = ("merge_n_" ++ a ++ "_"++ p)
                   , value = defEdVal
                   }
                 ]
        in
            { graph
                | nodes = n_ --List.sortWith (\m n -> NI.compIds m.v n.v) n_
                , edges = e_ --List.sortWith (\m n -> NI.compIds m.v n.v) e_
            }


removeMergeNode : String -> String -> GData -> GData
removeMergeNode a p graph =
    { graph
        | nodes = List.sortWith (\m n -> NI.compIds m.v n.v)
          (List.filter (\n ->
                            not <| (n.v == ("merge_a_" ++ p ++ "_"++ a) || n.v == ("merge_a_" ++ a ++ "_"++ p))
                       ) graph.nodes)
        , edges = List.sortWith (\m n -> NI.compIds m.v n.v)
          (List.filter
               (\e ->
                    not<| ( List.member e.w [("merge_a_" ++ p ++ "_"++ a), ("merge_a_" ++ a ++ "_"++ p)] )
               )
               graph.edges)
    }
        
        

            
removeSplitNode : String -> String -> GData -> GData
removeSplitNode a p graph =
    { graph
        | nodes = graph.nodes
        |> List.filter (\n -> not <| List.member n.v [("split_n_" ++ a), ("split_n_" ++ p)])
        |> List.sortWith (\m n -> NI.compIds m.v n.v)
        , edges = graph.edges
        |> List.filter (\n -> not <| List.member n.w [("split_n_" ++ a), ("split_n_" ++ p)])
        |> List.sortWith (\m n -> NI.compIds m.v n.v)
    }


    
addSplitNodes : String -> String -> GData -> GData
addSplitNodes a b graph =
    case ((List.filter (\n -> n.v == a) graph.nodes), (List.filter (\n -> n.v == b) graph.nodes)) of
        ([an], [bn]) ->
            if ((labelToData an.value.label |> List.length) > 1) && ((labelToData bn.value.label |> List.length) > 1) then
                
                { graph
                    | nodes = List.sortWith (\m n -> NI.compIds m.v n.v)
                      (graph.nodes ++
                           [ { v = ("split_n_" ++ a)
                             , value = { defNodeVal | label = "Split" }
                             }
                           , { v = ("split_n_" ++ b)
                             , value = { defNodeVal | label = "Split" }
                             }
                           ]
                      )
                    , edges = List.sortWith (\m n -> NI.compIds m.v n.v)
                      (graph.edges ++
                           [ { w = ("split_n_" ++ a)
                             , v = a
                             , value = defEdVal
                             }
                           , { w = ("split_n_" ++ b)
                             , v = b
                             , value = defEdVal
                             }
                           ]
                      )
                }

            else
                if (labelToData an.value.label |> List.length) > 1 then
                    
                    { graph
                        | nodes = List.sortWith (\m n -> NI.compIds m.v n.v)
                          (graph.nodes ++
                               [ { v = ("split_n_" ++ a)
                                 , value = { defNodeVal | label = "Split" }
                                 }
                               ]
                          )
                        , edges = List.sortWith (\m n -> NI.compIds m.v n.v)
                          (graph.edges ++
                               [ { w = ("split_n_" ++ a)
                                 , v = a
                                 , value = defEdVal
                                 }
                               ]
                          )
                    }
                else
                    
                    if (labelToData an.value.label |> List.length) > 1 then
                        
                        { graph
                            | nodes = List.sortWith (\m n -> NI.compIds m.v n.v)
                              (graph.nodes ++
                                   [ { v = ("split_n_" ++ b)
                                     , value = { defNodeVal | label = "Split" }
                                     }
                                   ]
                              )
                            , edges = List.sortWith (\m n -> NI.compIds m.v n.v)
                              (graph.edges ++
                                   [ { w = ("split_n_" ++ b)
                                     , v = b
                                     , value = defEdVal
                                     }
                                   ]
                              )
                        }
                    else
                        graph
        _ -> graph
