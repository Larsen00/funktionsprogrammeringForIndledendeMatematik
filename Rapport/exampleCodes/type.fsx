
let rec factorial n =
    match n with
    | 0             -> 1 
    | x when x > 0  -> x * factorial (x - 1)
    | _             -> failwith "Negative argument"


let f x y =
    sqrt <| float(factorial x + factorial y)

printfn "%A" (f 3 4)
let g  = f 3

