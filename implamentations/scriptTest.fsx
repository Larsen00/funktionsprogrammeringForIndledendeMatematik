#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open TreeGenerator
open Differentiation
open Expression
open Number
open SymbolicManipolation

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






let lhs = tree "a*4+b"
let rhs = N one

let (a, b) = isolateX lhs rhs (X 'a')
printfn "%A = %A" (ExpressionToInfix a false) (ExpressionToInfix b false)

