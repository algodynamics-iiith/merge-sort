module Config exposing (..)


type alias ItemCfg =
    { width : Float
    , height : Float
    , fill : String
    , stroke : String
    }

itemCfg =
    { width = 50
    , height = 50
    , fill = "#ffffff"
    , stroke = "#111111"
    }



type alias NodeCfg =
    { itemGap : Float
    , padding : Float
    }
    

    
nodeCfg =
    { itemGap = 20
    , padding = 20
    }

node_width : Int -> Float
node_width nItems =
    let all_items_width = nItems * itemCfg.width
        all_gaps_width = (nItems - 1) * nodeCfg.itemGap
        padding_width = nodeCfg.padding * 2
    in
        toFloat <| all_items_width + all_gaps_width + padding_width

            
node_height : Float
node_height =
    let padding_height = nodeCfg.padding * 2
    in
        itemCfg.height + padding_height


opNodeWidth : Float
opNodeWidth = 150


opNodeHeight : Float
opNodeHeight = 50
