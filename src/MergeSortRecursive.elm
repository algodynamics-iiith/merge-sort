port module MergeSortRecursive exposing (..)

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
import GraphInterface exposing (..)


import NodeId as NI
import LMerge as M
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
          , { v = "ms_n_$"
            , value = { defNodeVal | label = "MS" }
            }
          ]
    , edges =
          [ { v = "$"
            , w = "ms_n_$"
            , value = defEdVal
            }
          ]
    , value = {width = 0, height = 0}
    }


    
init : () -> ( Model, Cmd Msg )
init flags =
  ( { graph = ig1 []
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
                  , prompt = ("Click on the 'MS' button to expand the graph.",P.PromptInfo)
              }
            , sendMessage (gdEncode gr_)
            )

    Split np ->
        let graph = model.graph
            (n_, e_) = split np graph.nodes graph.edges
            gr_ = { graph
                      | nodes = List.sortWith (\m n -> NI.compIds m.v n.v) n_
                      , edges = List.sortWith (\m n -> NI.compIds m.v n.v) e_
                  }
        in
            ( { model
                  | graph = gr_
                  , prompt = if graph.edges |> canapply ("split_n_" ++ np) then
                                    (("Split [ "
                                         ++ ((getData np graph.nodes) |> List.map String.fromInt |> String.join " ")
                                         ++ " ]"
                                    ),P.PromptInfo)
                                else
                                    ("Cannot Apply.",P.PromptDanger)
              }
            , sendMessage (gdEncode gr_)
            )
        
    Expand np ->
        let graph = model.graph
            (n_, e_) = expand np graph.nodes graph.edges
            gr_ = { graph
                      | nodes = List.sortWith (\m n -> NI.compIds m.v n.v) n_
                      , edges = List.sortWith (\m n -> NI.compIds m.v n.v) e_
                  }            
        in
            ( { model
                  | graph = gr_
                  , prompt =
                    if graph.edges |> canapply ("ms_n_" ++ np) then
                        (("Expanding the mergesort [ "
                         ++ ((getData np graph.nodes) |> List.map String.fromInt |> String.join " ")
                         ++ " ] node."
                        ),P.PromptInfo)
                    else
                        ("Cannot apply this mergesort operation.  Split first.",P.PromptDanger)
              }
            , sendMessage (gdEncode gr_)
            )

    Merge np ->
        let graph = model.graph
            (n_, e_) = merge np graph.nodes graph.edges
            gr_ = { graph
                      | nodes = List.sortWith (\m n -> NI.compIds m.v n.v) n_
                      , edges = List.sortWith (\m n -> NI.compIds m.v n.v) e_
                  }
            medges = List.filter (\e -> e.w == ("merge_n_" ++ np)) graph.edges
            mnodes = List.filter (\n -> List.member n.v (List.map .v medges)) graph.nodes
        in
            ( { model
                  | graph = gr_
                  , prompt = if graph.edges |> canapply ("merge_n_" ++ np) then
                                    ("merge [ " ++ (String.join " ], [ " (List.map (\n -> n.value.label) mnodes)) ++ " ]",P.PromptInfo)
                                else
                                    ("Cannot apply this merge operation.",P.PromptDanger)
              }
            , sendMessage (gdEncode gr_)
            )


canapply : String -> List GDataEdge -> Bool
canapply nid el =
    el
        |> List.filter (\e -> e.w == nid)
        |> List.all (\e -> String.startsWith "$" e.v)
            

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
                  [ viewGraph graph ]
            ]            


viewGraph : GData -> Html Msg
viewGraph graph =
    let vh = 800
        vw = max 800 (graph.value.width * 1.1)
    in
        Svg.svg
            [ SA.viewBox <|
                  "0 0 "
                  ++ (String.fromFloat <| vh)
                  ++ " "
                  ++ (String.fromFloat <| vw)
                  
            , SA.width "700px"
            , HA.style "height" "100%"
            ]
            [ Svg.g
                  []
                  (List.map viewNode graph.nodes)
            , Svg.g
                  []
                  (List.map viewEdge graph.edges)
            ]



viewDataNode : GDataNode -> Svg Msg
viewDataNode node =
    let {value} = node

        h = value.height |> String.fromFloat
        w = value.width |> String.fromFloat

        bx = value.x - value.width/2 |> String.fromFloat
        by = value.y + 10 - value.height/2 |> String.fromFloat

        tx = (value.x - value.width / 2 + Config.nodeCfg.padding) |> String.fromFloat
        ty = (value.y + 10) |> String.fromFloat
        
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
                --, SA.fontFamily "mono0space"
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


                  
viewEdge : GDataEdge -> Svg Msg
viewEdge edge =
    let {value} = edge
        end = LE.last value.points |> Maybe.withDefault (Point 0 0)
        pre_end = LE.getAt ((List.length value.points) - 1) value.points
                |> Maybe.withDefault (Point 0 0)
        
    in
        Svg.g []
            [ Svg.polyline
                  [ SA.stroke "#9E9E9E"
                  , SA.strokeWidth "2"
                  , SA.fill "none"
                  , SA.points (pointsToString <| (List.take (List.length value.points - 1) value.points) ++ [Point end.x (end.y - 10)])
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
                            ]
                  ]
                  []
            ]       

