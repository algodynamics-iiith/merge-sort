module V1.Ftry exposing (..)
import V1.Function as F

type MSF = Split | Merge


split : List Int -> (List Int, List Int)
split l = (l, l)
