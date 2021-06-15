module NodeId exposing (compIds)


type Dir = Left
         | Right
           
type Func = Split
          | Merge
          | MS
          | NoOp

type AppStatus = App
               | NApp
            
type SortStatus = Sorted
                | NotSorted

             
type Sym = DirSym Dir
         | FuncSym Func
         | AppSym AppStatus
         | SortSym SortStatus
         | RootSym
           

{--

pt = root|l|r
p=(pt)+[/s]

merge_a_pt

merge_n_pt

ms_n_pt

ms_a_pt

split_pt

--}            

isDir : String -> Bool
isDir dstr =
    List.member dstr ["l", "r"]

        
dirFromStr : String -> Dir
dirFromStr dirstr =
    case dirstr of
        "l" -> Left
        _ -> Right


isFunc : String -> Bool
isFunc fstr =
    List.member fstr ["split", "merge", "ms"]

        
funcFromStr : String -> Func
funcFromStr funcstr =
    case funcstr of
        "split" -> Split
        "merge" -> Merge
        "ms" -> MS
        _ -> NoOp


isApp : String -> Bool
isApp astr =
    List.member astr ["a", "n"]
             
appFromStr : String -> AppStatus
appFromStr appstr =
    case appstr of
        "a" -> App
        "n" -> NApp
        _ -> NApp
             

isSort : String -> Bool
isSort sstr =
    List.member sstr ["/s"]
             
sortFromStr : String -> SortStatus
sortFromStr sstr =
    case sstr of
        "/s" -> Sorted
        _ -> NotSorted

             
symFromStr : String -> Maybe Sym
symFromStr symstr =
    if isFunc symstr then
        Just <| FuncSym (funcFromStr symstr)
    else
        if isDir symstr then
            Just <| DirSym (dirFromStr symstr)
        else
            if isApp symstr then
                Just <| AppSym (appFromStr symstr)
            else
                if isSort symstr then
                    Just <| SortSym (sortFromStr symstr)
                else
                    if symstr == "$" then
                        Just RootSym
                    else
                        Nothing


parsePath : String -> List Sym
parsePath pstr =
    if String.contains "/s" pstr then
        List.concat
            [ List.map
                  (\s ->
                       case symFromStr s of
                           Just sym -> sym
                           Nothing -> RootSym
                  )
                  (pstr |> String.dropRight 2 |> String.split "-")
            , [ SortSym Sorted ]
            ]
            
    else
        List.concat
            [ List.map
                  (\s ->
                       case symFromStr s of
                           Just sym -> sym
                           Nothing -> RootSym
                  )
                  (String.split "-" pstr)
            , [ SortSym NotSorted ]
            ]
            

parseId : String -> List Sym
parseId idstr =
    case String.split "_" idstr of
        
        "ms" :: (asym :: [path]) ->
            ( FuncSym MS
                  :: 
                      ( AppSym (appFromStr asym)
                      :: parsePath path
                      )
            )

        "merge" :: (asym :: [path]) ->
            ( FuncSym Merge
                  :: 
                      ( AppSym (appFromStr asym)
                      :: parsePath path
                      )
            )

        "split" :: (asym :: [path]) ->
            ( FuncSym Split
                  ::
                      ( AppSym (appFromStr asym)
                      :: parsePath path
                      )
            )            

        [p] -> parsePath p

        _ -> []


getPath : List Sym -> List Dir
getPath sl =
    case sl of
        
        RootSym :: restp ->
            List.map
                (\s ->
                     case s of
                         DirSym d -> d
                         _ -> Left
                )
                <| List.take ((List.length restp) - 1) restp
                    
        _ :: (_ :: (RootSym :: restp)) -> getPath (RootSym :: restp)
                                          
        _ -> []


revordr : Order -> Order
revordr ord =
    case ord of
        LT -> GT
        GT -> LT
        EQ -> EQ

              
comp : List Dir -> List Dir -> Order
comp p1 p2 =
    case (p1, p2) of
        
        ([], []) -> EQ

        (Left :: rest1, []) ->
            LT
                
        (Right :: rest1, []) ->
            GT

        ([], Right :: rest2) ->
            comp p2 p1 |> revordr

        ([], Left :: rest2) ->
            comp p2 p1 |> revordr

        (Left :: rest1, Left :: rest2) ->
            comp rest1 rest2

        (Right :: rest1, Right :: rest2) ->
            comp rest1 rest2
            
        (Left :: rest1, Right :: rest2) ->
            LT
            
        (Right :: rest1, Left :: rest2) ->
            GT

                
compIds : String -> String -> Order
compIds s1 s2 =
    comp
    (getPath (parseId s1))
    (getPath (parseId s2))
        
