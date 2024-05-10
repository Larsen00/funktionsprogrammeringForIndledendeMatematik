#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open TreeGenerator
open Differentiation
open Expression
open Number
open SymbolicManipolation
open rational
open complex
open commutativeAddSub
open commutativeMulDiv








// let ligning = simplifyExpr (tree "(3*x-5)*2")
// printfn "Orginal equation: %A" (InfixExpression ligning) 

// let dx = (diff ligning 'x') |> simplifyExpr
// printfn "Differentiated equation: %A" (InfixExpression dx)

let showSimp e =
    printfn "ORGINAL"
    printfn "%A" e
    printfn "\n%A" <| infixExpression e


    let ass = simplifyExpr e
    printfn "\nSIMPLIFIED"
    printfn "%A" ass
    printfn "\n%A" <| infixExpression ass
    ""

// let exp = Div
//             (Neg (Add (X 'I', Add (N (Int -9), X 'I'))), Div (Neg (N (Int 3)), N (Int -6)))

// let env = Map [('I', (Int 1))]
// printfn "%A" (simplifyExpr exp |> infixExpression)
// printfn "%A" (simplifyExpr exp)
// printfn "%A" (eval (simplifyExpr exp) env)


// printfn "%A" (simplifyExpr exp |> infixExpression |> tree |> infixExpression)
// printfn "%A" (simplifyExpr exp |> infixExpression |> tree)
// printfn "%A" (eval (simplifyExpr exp |> infixExpression |> tree ) env )



let e = tree "(3*y*(-2)/(-x))*x"
let ft = (commutativeMulDiv.flatTree e)
printfn "%A" e
printfn "%A" ft
printfn "%A" (sort ft)
printfn "%A" (commutativeMulDiv.applyCommutative e)

printfn "%A" <| tree "1--1"