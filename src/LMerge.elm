
module LMerge exposing (merge)

merge : List Int -> List Int -> List Int
merge a b = 
    case (a, b) of
        ([], []) -> []
        ([], hb::tb) -> hb::tb
        (ha::ta, []) -> ha::ta
        (ha::ta, hb::tb) -> 
            (if (ha < hb) then 
                ha :: (merge ta <| hb::tb)
             else hb :: (merge (ha::ta) tb))
