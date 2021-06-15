module MergeSublists exposing (..)

import Set exposing (Set)
import LMerge as C
import Browser
import Array exposing (Array)
import Random
import Random.Array as RA
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Prompt as P


sublist : Int -> Int -> List Int -> List Int
sublist i j l =
    l |> List.indexedMap (\idx val -> (idx, val))
      |> List.filter (\(idx, val) -> idx >= i && idx < j)
      |> List.map (\(idx, val) -> val)

-- state space

type alias Model = 
    { lst : List Int
    , bounds : Set Int
    , merged : Set Int
    , prompt : P.Prompt
    }

initPrompt =
    ("Select 2 adjacent sublists, by selecting 3 indices and perform Merge Operation",P.PromptInfo)

init : () -> ( Model, Cmd Msg )
init _ = 
    ( { lst = []
      , bounds = Set.fromList []
      , merged = Set.fromList []
      , prompt = initPrompt
      }
    , Cmd.batch
          [ Random.generate
                GotRandomArray
                (RA.rangeLengthArray 5 10
                     (Random.int 10 50))
          ]
    )

-- inputs

type Msg = Select Int 
         | Deselect Int
         | Merge
         | GotRandomArray (Array Int)

-- input to transition functions map

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Select i -> (select i model, Cmd.none)
        Deselect j -> (deselect j model, Cmd.none)
        Merge -> (merge model, Cmd.none)
        GotRandomArray a ->
            ( { model
                  | lst = Array.toList a
              }
            , Cmd.none
            )

-- transition functions for each input

addIndex : Int -> Set Int -> (Set Int, P.Prompt)
addIndex i bounds =
    if Set.size bounds == 3 then 
        (bounds,("Click Merge to merge the adjacent sublists",P.PromptInfo)) 
    else if Set.size bounds == 2 then
        (Set.insert i bounds,("Click Merge to merge the adjacent sublists",P.PromptInfo))
    else
        (Set.insert i bounds,("Select "++(String.fromInt (2 - Set.size bounds))++ " more index, to perform merge.",P.PromptInfo))


select : Int -> Model -> Model
select i model =
    let
        (newBounds,newprompt) = model.bounds |> addIndex i
    in
    {model | bounds = newBounds,merged = Set.fromList [], prompt=newprompt }
removeIndex : Int -> Set Int -> (Set Int,P.Prompt)
removeIndex i bounds =
    let
        newBounds = Set.remove i bounds
    in
        (newBounds,("Select "++(String.fromInt (3 - Set.size newBounds))++ " more index, to perform merge.",P.PromptInfo) )

deselect : Int -> Model -> Model
deselect i model =
    let
        (newBounds,newprompt) = model.bounds |> removeIndex i
    in
    {model | bounds = newBounds, prompt=newprompt }
mergeSublistsAt : Int -> Int -> Int -> List Int -> List Int
mergeSublistsAt i j k lst =
    List.concat [ (sublist 0 i lst)
                , C.merge (sublist i j lst) (sublist j k lst)
                , (sublist k (List.length lst) lst)
                ]

merge : Model -> Model
merge model =
    let {lst, bounds} = model
        indices = bounds |> Set.toList |> List.sort
    in
        case indices of
            [i, j, k] -> 
                { model | lst = lst |> mergeSublistsAt i j k
                        , bounds = Set.empty
                        , merged = Set.fromList [i, k]
                        , prompt = initPrompt 
                }
            _ -> model


gridLine : Int -> Bool -> Html Msg
gridLine position isSelected = 
    Html.div
        [HA.class "flex flex-col"]
        [ Html.div
              [ HA.class "flex-grow cursor-pointer"
              , HA.style "background" (if isSelected then "#FF5722" else "#B2EBF2") 
              , HA.style "padding" "0.5em"
              , HE.onClick (if isSelected then (Deselect position) else (Select position))
              ][]
        , Html.div
              [ HA.class "text-center"
              ]
              [ Html.text <| String.fromInt position
              ]
        ]


selected : Int -> Set Int -> Bool
selected i bounds =
    Set.member i bounds


inbound i bounds =
    let blist = bounds |> Set.toList
    in
        case ((List.minimum blist), (List.maximum blist)) of
            (Nothing, Nothing) -> False
            (Just mn, Just mx) -> (i >= mn && i < mx)
            _ -> False



itemview : Int -> Int -> Int -> Set Int -> Set Int -> Html Msg
itemview val index length indices merged =
    let
        (bgClr,td) = if inbound index indices then 
                    ("#F0F4C3","1s") 
                else if inbound index merged then
                    ("#B0EEAC","5s")    
                else
                    ("#ffffff","1s")
    in
    if index == length - 1 then
        Html.div
            [ HA.style "display" "flex"
            ]
            [ gridLine index <| selected index indices
            , Html.div
                [ HA.style "margin" "0.25em"
                , HA.style "margin-bottom" "1.5em"
                , HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "align-items" "center"
                , HA.style "background" bgClr
                -- , HA.style "transition" ("all " ++ td ++ " ease-in-out")
                ]
                [ Html.span 
                    [ HA.style "padding" "0.4em"
                    , HA.class "text-2xl"
                    ] 
                    [Html.text <| String.fromInt val]
                ]
            , gridLine (index + 1) <| selected (index + 1) indices
            ]
    else
        Html.div
            [ HA.style "display" "flex"
            ]
            [ gridLine index <| selected index indices
            , Html.div
                [ HA.style "margin" "0.25em"
                , HA.style "margin-bottom" "1.5em"
                , HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "align-items" "center"
                , HA.style "background" bgClr
                -- , HA.style "transition" ("all " ++ td ++ " ease-in-out")
                ]
                [ Html.span 
                    [ HA.style "padding" "0.4em"
                    , HA.class "text-2xl"
                    ] 
                    [Html.text <| String.fromInt val]
                ]
            ]

view model = 
    Html.div
        [ HA.class "flex-grow"
        , HA.class "flex"
        , HA.class "flex-col"
        , HA.class "justify-center"
        ]
        [ P.show model.prompt

        , Html.div
              [HA.class "flex-grow flex flex-col justify-around py-20"]
              [ Html.div
                    [HA.class "flex justify-center"]
                    (List.indexedMap
                         (\ i v ->
                              itemview v i (List.length model.lst) model.bounds model.merged
                         )
                         model.lst
                    )
              , Html.div 
                [HA.class "flex flex-row justify-around h-8"]
                        (List.indexedMap (\p e -> Html.div [] [(Html.text <| (String.fromChar (Char.fromCode (105+p))) ++ " = " ++ (String.fromInt e))]) (Set.toList model.bounds))
                    
              ]
        , Html.div
              [ HA.class "flex-grow"
              , HA.class "flex"
              , HA.class "justify-evenly"
              , HA.class "items-start"
              , HA.class "bg-gray-300"
              , HA.class "py-4"
              ]
              [ Html.button
                    [ HE.onClick Merge
                    , HA.class "bg-gray-600"
                    , HA.class "text-gray-100"
                    , HA.class "p-2"
                    ]
                    [Html.text "merge"]
              ]
        ]


   

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
        
