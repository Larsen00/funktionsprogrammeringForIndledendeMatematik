
#load "SymbolicManipolation.fsx"
open Expression
open Number


open Differentiation
open TreeGenerator
open SymbolicManipolation

// mangler simplificierns regler Mul (N (Int 2), Mul (N (Int 2), X 'x'))


let expr0 = (Mul (Mul (N (Int 2), Mul (N (Int 3), X 'y')), Mul (N (Int 4), Mul (N (Int 2), Mul (N (Int 2), X 'x')))))
//printfn "%A" (eval expr0 Map.empty)


// tror der sker stack overflow
let map = Map.ofList [('x', N (Int 1))]
let expr1 = (Mul (Mul (expr0, Mul (Mul (N (Int 2), X 'x'), X 'y')), Mul (N (Int 4), Mul (N (Int 2), Mul (N (Int 2), X 'x')))))
//printfn "%A" (eval expr1 Map.empty)
//printfn "%A" (eval expr1 map)

let expr2 = Mul (N (Int 3), Mul (N (Int 2), X 'x'))
let expr3 = Mul(expr2 ,Mul(expr2, expr2))
//printfn "%A" (eval expr1 Map.empty)
//printfn "%A" (eval expr3 Map.empty)


let expr4 = tree "x*x+y*x/3"
let d = diff expr4 'x'
printfn "%A" expr4
printfn "DIFF"
printfn "%A" d
printfn "Simplified"
let evaled = (eval d Map.empty)
printfn "%A" evaled


printfn "%A" (ExpressionToInfix evaled)