type faver = | Sort | Hvid | G | T | W 
let rec erFarvenSort fave =
    
    match fave with
    | Sort ->
        printfn "B  D" 
        erFarvenSort W
    | _ -> 
        printfn " == "
        erFarvenSort Sort

printfn "%A" (erFarvenSort Sort)