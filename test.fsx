
#load "SymbolicManipolation.fsx"
// Modules
open Expression
open Number
open rantionalAndComplex

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


// let expr1 = tree "x*2"
// let expr2 = tree "-y*2"
// let expr3 = tree "10*x+1"
// let expr4 = Mul(expr1, expr2)
// let expr5 = Add(expr3, expr4)
// let expr6 = Div(expr4, expr5)
// let expr7 = Mul(expr6, expr5)

// showSimp expr7

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


// let ligning = tree "(3*x-5)*2"
// let rat = N (Rational (R(3, 2)))
// let i = N (Int 5)
// showSimp (Add(ligning, rat))

// printfn "\nEvaluation x = 2:"
// printfn "%A" (eval (Add(ligning, rat)) (Map.ofList [('x', N (Int 2))]))


// showSimp (Sub (N (Rational (R (-9, 7))), X 'T'))
// Simplified: Mul (X 'Q', Div (N (Int 1), N (Int -3)))
// map [('O', Rational (R (-1, 1))); ('T', Int 8)], ['O'; 'T']

let generateExpresion = 
                            

                                    Sub (N (Rational (R (-9, 5))), N (Rational (R (0, 1))))


let m = Map.ofList []
let s = simplifyExpr generateExpresion
// let a = (AssociationForAddSub.applyAssociation (generateExpresion))
// showSimp generateExpresion
// printfn "%A" (eval s m)
printfn "%A" (eval generateExpresion m)
printfn "%A" (eval s m)

// Add
//   (Div
//      (Mul (Neg (N (Rational (R (5, 7)))), X 'U'),
//       Neg (Neg (N (Rational (R (-1, 4)))))),
//    Sub
//      (Add
//         (Div (N (Rational (R (8, 9))), N (Rational (R (-4, 1)))),
//          Div (X 'U', N (Rational (R (9, 2))))),
//       Add (Neg (X 'A'), Add (N (Rational (R (4, 3))), N (Rational (R (-7, 6)))))))


// flattree [Div (Mul (N (Int 5), X 'U'), N (Rational (R (-1, 4)))); Mul (N (Int 1), X 'D');
//  Div (X 'U', N (Rational (R (9, 2)))); Neg (X 'A'); N (Rational (R (4, 3)));
//  N (Rational (R (-1, 2)))]
 
// sortAss [N (Rational (R (4, 3))); N (Rational (R (-1, 2))); Mul (N (Int 1), X 'D');
//  Div (Mul (N (Int 5), X 'U'), N (Rational (R (-1, 4))));
//  Div (X 'U', N (Rational (R (9, 2)))); Neg (X 'A')]


