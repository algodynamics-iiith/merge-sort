module MergeAlgo exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Browser
import Array exposing (Array)
import Random
import Random.Array as RA
import Prompt as P


type alias Model =
    { firstList: List Int
    , secondList: List Int
    , mergedList: List Int
    , prompt: P.Prompt
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { firstList = []
      , secondList = []
      , mergedList = []
      , prompt = ("""Click on NEXT buttons to move 
                      the first number from the appropriate 
                      list to the end of the 'merged' list.""", P.PromptInfo)
      }
    , Cmd.batch
          [ Random.generate
                GotRandomArray
                ( Random.pair
                      ( Random.pair
                            (RA.rangeLengthArray 2 8 (Random.int 10 50))
                            (RA.rangeLengthArray 2 8 (Random.int 10 50))
                      )
                      (Random.weighted (80, False) [(20, True)])
                )
          ]
    )
    
    
type Msg = Next
         | NoOp
         | GotRandomArray ((Array Int, Array Int), Bool)

    
{-- 
move one element from start of given list
to end of merge list.
--}
move : List Int -> List Int -> (List Int, List Int)
move from ml =
    case from of
        [] -> (from, ml)
        h :: rest -> (rest, ml ++ [h])

                     
isLeftGreater: List Int -> List Int -> Bool
isLeftGreater l r =
    case (List.head l, List.head r) of
        
        (Just lh, Just rh) ->
            lh > rh

        (Just lh, Nothing) ->
            True

        _ ->
            False
                     
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        
        NoOp ->
            (model, Cmd.none)
                
        Next ->
            case (model.firstList, model.secondList) of
                
                ([], []) ->
                    ( { model
                          | prompt = ("""Merging is complete.""",P.PromptSuccess)
                      }
                    , Cmd.none
                    )
                    
                ([], _) ->
                    let (sl, ml) = move model.secondList model.mergedList                        
                    in
                        ( { model
                              | secondList = sl
                              , mergedList = ml
                              , prompt = if List.isEmpty sl then ("Merging is complete",P.PromptSuccess) else model.prompt
                          }
                        , Cmd.none
                        )

                (_, []) ->
                    
                    let (fl, ml) = move model.firstList model.mergedList
                    in
                        ( { model
                              | firstList = fl
                              , mergedList = ml
                              , prompt = if List.isEmpty fl then ("Merging is complete",P.PromptSuccess) else model.prompt
                          }
                        , Cmd.none
                        )
                _ ->
                    if not (isLeftGreater model.firstList model.secondList) then
                        let (fl, ml) = move model.firstList model.mergedList
                        in
                            ( { model
                                  | firstList = fl
                                  , mergedList = ml
                              }
                            , Cmd.none
                            )
                    else
                        let (sl, ml) = move model.secondList model.mergedList
                        in
                            ( { model
                                  | secondList = sl
                                  , mergedList = ml
                              }
                            , Cmd.none
                            )

        GotRandomArray ((a, b), dosort) ->
            ( { model
                  | firstList = List.sort <| Array.toList a
                  , secondList = List.sort <| Array.toList b
              }
            , Cmd.none
            )
                

arrayItem : Int -> Html Msg
arrayItem v =
    Html.span
        [ HA.style "padding" "1em 0.2em" ]
        [ Html.text <| String.fromInt v ]    


issorted : List Int -> List Int -> Bool
issorted l1 l2 =
    List.all (\x -> x == True) <| List.map2 (\i1 i2 -> i1 == i2) l1 l2
            
            
listView : String -> List Int -> Html Msg
listView label lst =
    Html.div
        [ HA.style "flex-grow" "1"
        , HA.class "flex"
        , HA.class "flex-wrap"
        , HA.class "justify-center"
        , HA.class "items-center"
        , HA.class "text-xl"
        , HA.class "py-4"
        ]
    [ Html.span [][Html.text (label ++ ":")]
    , Html.div
          [ HA.classList
                [ ("bg-green-200", issorted lst (List.sort lst))
                , ("bg-red-200", not <| issorted lst (List.sort lst))
                ]
          , HA.class "ml-4"
          , HA.class "py-2"
          ]
          ( [ Html.span
                  [ HA.class "px-2"]
                  [ Html.text "[" ]
            ]
                ++
                ( lst
                |> List.map arrayItem
                |> List.intersperse (Html.span [ HA.class "pr-2" ] [Html.text ","])
                )
                ++
                [ Html.span
                      [ HA.class "px-2" ]
                      [Html.text "]"]
                ]
          )
    ]

    
view: Model -> Html Msg
view model =
    Html.div
        [ HA.class "flex-grow"
        , HA.class "h-full"
        , HA.class "flex"
        , HA.class "flex-col"
        , HA.class "justify-center"
        ]
        [ P.show model.prompt
        , Html.div
              [ HA.class "flex"
              , HA.class "flex-grow"
              , HA.class "flex-wrap"
              , HA.class "items-center"
              ]
              [ listView "left" model.firstList
              , listView "right" model.secondList
              ]
              
        , Html.div
              [ HA.class "flex-grow" ]
              [ listView "merged" model.mergedList ]
                  
        , let lh = List.head model.firstList
              rh = List.head model.secondList
          in
              case (lh, rh) of

                  (Nothing, Nothing) ->
                      controls True

                  _ -> controls False
        ]



    

        
controls : Bool -> Html Msg
controls complete =
    Html.div
        [ HA.class "flex-grow"
        , HA.class "flex"
        , HA.class "justify-evenly"
        , HA.class "items-start"
        , HA.class "bg-gray-300"
        , HA.class "py-4"
        ]
    [ Html.button
          [ HE.onClick ( if complete then NoOp else Next )
          , HA.class ( if not complete then "bg-gray-600" else "bg-gray-400" )
          , HA.class "text-gray-100"
          , HA.class "p-2"
          ]
          [ Html.text "NEXT"
          ]
    ]
    

main : Program () Model Msg
main = Browser.element
       { init = init
       , update = update
       , view = view
       , subscriptions = (\_ -> Sub.none)
       }
