


let rec factorial n =
    match n with
    | _ when n <0 -> failwith ""
    | 0 | 1 -> 1 
    | _ -> n * factorial (n - 1) 

// Eksempel på brug
let result = factorial 5
printfn "Factorial of 5 is: %d" result

let (var1, var2) as tuple1 = (1, 2)
printfn "%d %d %A" var1 var2 tuple1