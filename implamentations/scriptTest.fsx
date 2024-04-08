#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open TreeGenerator
open Differentiation
open Expression
open Number
open SymbolicManipolation
open rational
open complex

// let ligning = simplifyExpr (tree "(3*x-5)*2")
// printfn "Orginal equation: %A" (InfixExpression ligning) 

// let dx = (diff ligning 'x') |> simplifyExpr
// printfn "Differentiated equation: %A" (InfixExpression dx)

let showSimp e =
    printfn "ORGINAL"
    printfn "%A" e
    printfn "\n%A" (ExpressionToInfix e false)


    let ass = simplifyExpr e
    printfn "\nSIMPLIFIED"
    printfn "%A" ass
    printfn "\n%A" (ExpressionToInfix ass false)
    ""


let e = Add (X 'E', Neg (Sub (Sub (N (Int 10), N (Int -1)), Mul (X 'R', X 'R'))))
let env = Map [('A', Complex (C (R (0, 1), R (-2, 1))));
    ('E', Complex (C (R (3, 2), R (2, 1))));
    ('R', Complex (C (R (1, 1), R (3, 1)))); ('U', Rational (R (1, 1)))]


let s0 = eval e env 
let se = simplifyExpr e
let t =  InfixExpression se
let s1 = eval (t|> tree) env

printfn "s0:\n%A" s0
printfn "s1\n%A" s1

printfn "%A" (InfixExpression e)
printfn "%A" (tree t |> InfixExpression)
printfn "%A" se
