(*#load "modules/Rational.fs"
#load "modules/Number.fs"
#load "modules/Expression.fs"
#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open Rational
open Number
open Expression
*)
#load "Differentiation.fsx"
open Expression
open Number

let map = Map.ofList [('x', N (Int 5))]
//Map.tryFind (X 'x') map

// tror man bør indsætte mappet før man simplificere
let rec eval e map =
    match e with
    | X a       -> 
                match Map.tryFind a map with
                | None -> e
                | Some a -> a
    | N _       -> e
    | Neg a     -> - eval a map // missing simplified rules
    | Add(a, b) -> eval a map + eval b map
    | Sub(a, b) -> eval a map - eval b map
    | Mul(a, b) -> eval a map * eval b map
    | Div(a, b) -> eval a map / eval b map


// TEST
//let x = eval (Add (N (Int 0), (N (Int 10)))) map
//printfn "%A" x