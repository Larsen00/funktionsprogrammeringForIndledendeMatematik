#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
#r "nuget: FsCheck"
open FsCheck
open Expression
open Number
open Generators
open SymbolicManipolation
open TreeGenerator


Arb.register<SmallEnvGen>()

// Compares the evaluation of a simplified expression with the evaluation of the original expression
let compareSimpExpr env (e:Expr<Number>) =
    // printfn "Expression: %A" e
    eval (simplifyExpr e) env  = eval e env


// samples and expression and test if the simplified expression is equal to the original expression
let simpEqualEval (env, xlist) = 
    try
        if Gen.sample 1 1 (exprGen xlist 10 leafGen) |> List.head |> compareSimpExpr env then 1 else 0
    with
        | :? System.DivideByZeroException as _ -> 2
        | :? System.OverflowException as _ -> 3


let simpPBT (se:SmallEnv) =
    let result = simpEqualEval se
    (result = 1 || result = 2 || result = 3)
    |> Prop.classify (result = 1) "Equal"
    |> Prop.classify (result = 2) "DivideByZeroExceptions"
    |> Prop.classify (result = 3) "OverflowException"


let _ = Check.Quick simpPBT

let generatesCorrectTree env (e:Expr<Number>) =
    // printfn "Expressionnos:\n%A" e
    // printfn "Expression:\n%A" (simplifyExpr e)
    eval e env = eval (simplifyExpr e |> InfixExpression |> tree ) env

let treeEqualEval (env, xlist) =
    try 
        if Gen.sample 1 1 (exprGen xlist 10 onlyIntleafGen) |> List.head |> generatesCorrectTree env then 1 else 0
    with
        | :? System.DivideByZeroException as _ -> 2
        | :? System.OverflowException as _ -> 3

let treePBT (se:SmallEnv) =
    let result = treeEqualEval se
    (result = 1 || result = 2 || result = 3)
    |> Prop.classify (result = 1) "Equal"
    |> Prop.classify (result = 2) "DivideByZeroExceptions"
    |> Prop.classify (result = 3) "OverflowException"

let _ = Check.Quick treePBT