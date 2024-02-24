
#load "SymbolicManipolation.fsx"
// Modules
open Expression
open Number
open rational

// fsx files
open Differentiation
open TreeGenerator
open SymbolicManipolation

let showSimp e =
    printfn "ORGINAL"
    printfn "%A" e
    printfn "\n%A" (ExpressionToInfix e false)


    let ass = simplifyExpr e
    printfn "\nSIMPLIFIED"
    printfn "%A" ass
    printfn "\n%A" (ExpressionToInfix ass false)
    ""


let expr1 = tree "x*2"
let expr2 = tree "-y*2"
let expr3 = tree "10*x+1"
let expr4 = Mul(expr1, expr2)
let expr5 = Add(expr3, expr4)
let expr6 = Div(expr4, expr5)
let expr7 = Mul(expr6, expr5)

showSimp expr7

//let dx = (diff expr5 'x')
//let simplified = simplifyExpr dx

//let expr5eval = simplifyExpr (insert expr5 (Map.ofList [('x', N (Int 2)); ('y', N (Int -1))]))




(*
printfn "Orginal Equation:\n%A\n" (ExpressionToInfix expr5 false)
printfn "Simplified Diff x Equation:\n%A\n" (ExpressionToInfix simplified false)
printfn "Orginal Equaltion evaled with x = 2, y = -1:\n%A\n" (ExpressionToInfix expr5eval false)

printfn "%A" expr5eval

printfn "%A" dx
*)


let ligning = tree "(3*x-5)*2"
let rat = N (Rational (R(3, 2)))
let i = N (Int 5)
showSimp (Add(ligning, rat))
