module Prompt exposing (..)

import Html exposing (Html)
import Html.Attributes as HA

type PromptType =
    PromptSuccess
    | PromptDanger
    | PromptInfo

type alias Prompt =
    (String, PromptType)


colorScheme : PromptType -> ((String,String),String)
colorScheme promptType = 
    case promptType of
        PromptSuccess ->
            (("#155724", "#d4edda"), "#c3e6cb")
        PromptDanger ->
            (("#721c24", "#f8d7da"), "#f5c6cb")
        PromptInfo ->
            (("#004085", "#cce5ff"), "#b8daff")

show : Prompt -> Html msg
show (prompt_text,promptType) =
    let
        ((clr,bgClr),bdrClr) = colorScheme promptType
    in
        Html.div
            [ HA.style "display" "flex"
            , HA.style "justify-content" "center"
            , HA.style "align-items" "center"
            , HA.style "font-family" "sans-serif"        
            , HA.style "color" clr
            , HA.style "background" bgClr
            , HA.style "border-color" bdrClr
            , HA.style "font-size" "1em"
            , HA.style "box-sizing" "border-box"
            , HA.style "border" "1px solid"
            , HA.style "border-radius" "0.25rem"
            , HA.style "padding" "0.75rem 1.25rem"
            , HA.style "margin" "0.5rem"
            ]
            [Html.text prompt_text]