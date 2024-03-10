#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
#r "nuget: FsCheck"
#load "Generators.fsx"
open FsCheck
open rantionalAndComplex
open Number
open Expression
open Generators


// Compares the evaluation of a simplified expression with the evaluation of the original expression
let compareSimpExpr env (e:Expr<Number>) =
    // printfn "Expression: %A" e
    eval (SymbolicManipolation.simplifyExpr e) env  = eval e env


// samples and expression and test if the simplified expression is equal to the original expression
let simpEqualEval (env, xlist) = 
    try
        if Gen.sample 1 1 (exprGen xlist 10) |> List.head |> compareSimpExpr env then 1 else 0
    with
        | :? System.DivideByZeroException as ex ->
            printfn "DivideByZeroException: %A" ex
            2
        | :? System.OverflowException as ex ->
            printfn "OverflowException: %A" ex
            3

let simpPBT (se:SmallEnv) =
    let result = simpEqualEval se
    (result = 1 || result = 2 || result = 3)
    |> Prop.classify (result = 1) "Equal"
    |> Prop.classify (result = 2) "DivideByZeroExceptions"
    |> Prop.classify (result = 3) "OverflowException"


let _ = Check.Quick simpPBT

