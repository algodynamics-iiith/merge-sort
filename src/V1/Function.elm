module V1.Function exposing ( Function, FuncApplicationState
                            , prim, composite, apply, app
                            , add, mult, appliedAddFn
                            )


type FuncApplicationState = Applicable | Applied

type alias FnWithState f = 
    { fn : f
    , state : FuncApplicationState
    }


fnWithState : f -> FuncApplicationState -> FnWithState f
fnWithState f1 s = 
    { fn = f1
    , state = s
    }

type Function f = Primitive (FnWithState f)
                | Composite (FnWithState f)

prim : a -> Function a
prim p = Primitive (fnWithState p Applicable)

composite : a -> Function a
composite c = Composite (fnWithState c Applicable)

apply : FnWithState f -> FnWithState f
apply fns = 
    let {state} = fns
    in
        {fns | state = Applied}


app : Function f -> Function f
app fn =
    case fn of
        Primitive fns -> fns |> apply |> Primitive
        Composite fns -> fns |> apply |> Composite

-- primitive function
add : Int -> Int -> Int
add x y = x + y


-- composite function : composed of add and mult
mult : Int -> Int -> Int
mult a b = 
    if a == 1 then 
        b
    else
        add b (mult (a - 1) b)

addFn = prim add -- Primitive {fn: (Int -> Int -> Int), state: Applicable} : Function (Int -> Int -> Int)

multFn = composite mult -- Composite {fn: (Int -> Int -> Int), state: Applicable} : Function (Int -> Int -> Int)

appliedAddFn = app addFn -- Primitive {fn: (Int -> Int -> Int), state: Applied} : Function (Int -> Int -> Int)
