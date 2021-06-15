port module MergeSortAlgo exposing (..)

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
import MergeSortAlgoGraph as MG exposing (..)

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
  ( { graph = ig1 [] }
  , Cmd.batch
        [ Random.generate
            GotRandomArray
            (RA.rangeLengthArray 5 10
                 (Random.int 10 50))
        ]
  )


-- UPDATE


type Msg
  = Send
  | Recv String
  | GraphMsg MG.Msg
  | GotRandomArray (Array Int)


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

    GraphMsg dm ->
        let gr_ = MG.update dm model.graph
        in
            ( {model
                  | graph = gr_
              }
            , sendMessage (gdEncode gr_)
            )
            
    GotRandomArray arr ->
        let gr_ = ig1 <| Array.toList arr
        in
            ( { model
                  | graph = gr_
              }
            , sendMessage (gdEncode gr_)
            )
            

view : Model -> Html Msg
view model =
    let {graph} = model
    in
        Html.div
            [ HA.style "background" "#FAFAFA"
            , HA.style "display" "flex"
            , HA.style "margin" "10px"
            ]
            [ viewGraph graph
            ]



viewGraph : GData -> Html Msg
viewGraph graph =
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
            , HA.style "border" "1px solid"
            , HA.style "background" "#FFFFFF"
            , HA.style "height" "100%"
            ]
            [ Html.map (\m -> GraphMsg m) <| Svg.g [] (List.map viewNode graph.nodes)
            , Html.map (\m -> GraphMsg m) <| Svg.g [] (List.map viewEdge graph.edges)
            ]                        
