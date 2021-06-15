port module V1.MS exposing (..)

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

import V1.MergeSortGraph as MG
import V1.LayoutGraph as LG
import V1.Converter as C
import Graph as G

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model = 
    { cg: MG.MSGraph
    , lg: LG.LayoutGraph
    }

init : () -> ( Model, Cmd Msg )
init flags =
    let cg_ = MG.startComputation
        lg_ = C.toLg cg_
    in
        ( { cg = cg_
          , lg = lg_
          }
        , Cmd.batch
            [ Random.generate
                GotRandomArray
                 (RA.rangeLengthArray 5 10
                     (Random.int 10 50))
            , sendGraph <| LG.lgEncode lg_
            ]
        )

port sendGraph : Value -> Cmd msg

port receiveGraph : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveGraph Recv

type Msg
  = Send
  | Recv String
  | GotRandomArray (Array Int)
  | Expand String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    Send ->
      ( model
      , sendGraph (LG.lgEncode model.lg)
      )

    Recv gstr ->
        let res = 
                D.decodeString LG.lgDecoder gstr
        in
            case res of
                Ok lg_ -> 
                    ( { model
                          | lg = lg_
                      }
                    , Cmd.none                      
                    )
                Err e -> (model, Cmd.none)


    Expand n ->
        let id = String.toInt n |> Maybe.withDefault -1
            cg_ = MG.expandfnode id model.cg
            lg_ = C.toLg cg_
        in
            ( { model 
              | cg = cg_
              , lg = lg_
              }
            , sendGraph (LG.lgEncode lg_)
            )


    _ -> (model, Cmd.none)

view : Model -> Html Msg
view model =
    let {cg, lg} = model
    in
        Html.div
            [ HA.style "background" "#FAFAFA"
            , HA.style "display" "flex"
            , HA.style "margin" "10px"
            ]
            [ viewGraph cg lg
            ]


viewGraph : MG.MSGraph -> LG.LayoutGraph -> Html Msg
viewGraph cg lg =
    let vh = max 800 (lg.value.height * 1.1)
        vw = max 800 (lg.value.width * 1.1)
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
            [ Svg.g [] (List.map (viewNode cg) lg.nodes)
            , Svg.g [] (List.map (viewEdge cg) lg.edges)
            ]


viewNode : MG.MSGraph -> LG.Node -> Svg Msg
viewNode cg n =
    let cgnode_context = G.get (Maybe.withDefault -1 (String.toInt n.v)) cg
    in
        case cgnode_context of
            Just nc ->
                case nc.node.label of
                    MG.DataNode d -> viewDataNode cg n
                    MG.FuncNode l f -> viewFuncNode cg n
            Nothing -> Svg.rect [] []


viewDataNode : MG.MSGraph -> LG.Node -> Svg Msg
viewDataNode cg n = 
    let 
        ctnt =
            Svg.text_
                [ SA.x (n.value.x |> String.fromFloat)
                , SA.y (n.value.y |> String.fromFloat)
                , SA.fontSize (String.fromInt 25)
                ]
                [Svg.text n.value.label]
        in
            Svg.g [] [ctnt]


viewFuncNode : MG.MSGraph -> LG.Node -> Svg Msg
viewFuncNode cg n =
    let box = 
            Svg.rect 
                [ SA.x (n.value.x |> String.fromFloat)
                , SA.y (n.value.y |> String.fromFloat)
                , SA.width (n.value.width |> String.fromFloat)
                , SA.height (n.value.height |> String.fromFloat)
                , SE.onClick <| Expand n.v
                ]
                []

        ctnt =
            Svg.text_
                [ SA.x (n.value.x |> String.fromFloat)
                , SA.y (n.value.y |> String.fromFloat)
                ]
                [Svg.text n.value.label]
        in
            Svg.g [] [box, ctnt]



viewEdge : MG.MSGraph -> LG.Edge -> Svg Msg
viewEdge cg e = 
    Svg.polyline 
        [ SA.points (String.join " " <| List.map 
                                        (\p -> ((String.fromFloat p.x)
                                               ++ ","
                                               ++ (String.fromFloat p.y))) e.value.points)
        , SA.strokeWidth "10"
        , SA.stroke "black"
        , SA.fill "none"
        ]
        []
