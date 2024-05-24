#r "../bin/Release/net7.0/main.dll"
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
    eval (simplifyExpr e) env  = eval e env


// samples and expression and test if the simplified expression is equal to the original expression
let simpEqualEval (env, xlist) = 
    try
        if Gen.sample 1 1 (exprGen xlist 10 leafGen) 
            |> List.head 
            |> compareSimpExpr env 
        then 1 else 0
    with
        | :? System.DivideByZeroException as _ -> 2
        | :? System.OverflowException as _ -> 3


let simpPBT (se:SmallEnv) =
    let result = simpEqualEval se
    (result = 1 || result = 2 || result = 3)
    |> Prop.classify (result = 1) "Equal"
    |> Prop.classify (result = 2) "DivideByZeroExceptions"
    |> Prop.classify (result = 3) "OverflowException"

printfn "Simplified expression equals original expression"
Arb.register<SmallEnvGen>()
let _ = Check.Quick simpPBT

let generatesCorrectTree env (e:Expr<Number>) =
    eval e env = eval 
        (simplifyExpr e 
        |> infixExpression 
        |> tree 
        |> infixExpression 
        |> tree ) env

let treeEqualEval (env, xlist) =
    try 
        if Gen.sample 1 1 (exprGen xlist 10 onlyIntleafGen) 
            |> List.head 
            |> generatesCorrectTree env 
        then 1 else 0
    with
        | :? System.DivideByZeroException as _ -> 2
        | :? System.OverflowException as _ -> 3

let treePBT (se:SmallEnv) =
    let result = treeEqualEval se
    (result = 1 || result = 2 || result = 3)
    |> Prop.classify (result = 1) "Equal"
    |> Prop.classify (result = 2) "DivideByZeroExceptions"
    |> Prop.classify (result = 3) "OverflowException"

printfn "\nGenerates correct tree"
Arb.register<SmallEnvGen>()
let _ = Check.Quick treePBT


// Eval PBT
let evalOperation e1 e2 env f =
   eval (f e1 e2) env = (getNumber <| f (eval e1 env |> N) (eval e2 env |> N))

let evalPBT ((env ,xlist):SmallEnv) = 
    let result = 
        try
            let exprList = Gen.sample 1 2 (exprGen xlist 10 leafGen)
            let e1::[e2] = exprList
            let prop = evalOperation e1 e2 env
            let negation = eval (-e1) env = - eval e1 env
            if negation && prop ( + ) && prop ( - ) && prop ( * ) && prop ( / ) then 1 else 0
        with
        | :? System.DivideByZeroException as _ -> 2
        | :? System.OverflowException as _ -> 3
    (result = 1 || result = 2 || result = 3)
    |> Prop.classify (result = 1) "Property Holds"
    |> Prop.classify (result = 2) "DivideByZeroExceptions"
    |> Prop.classify (result = 3) "OverflowException"

printfn "\nhomophi egnenskab eval (e1 + e2) env = eval e1 env + eval e2 env"
let _ = Check.Quick evalPBT


// divisin by zero test
let div0 ((env ,xlist):SmallEnv) = 
    let result = 
        try 
        let  e = Gen.sample 1 1 (exprGen xlist 10 leafGen) |> List.head
        let _ = eval e env
        1
        with 
            | :? System.DivideByZeroException as _ -> 2
            | :? System.OverflowException as _ -> 3
    (result = 1 || result = 2 || result = 3)
    |> Prop.classify (result = 1) "Fine"
    |> Prop.classify (result = 2) "DivideByZeroExceptions"
    |> Prop.classify (result = 3) "OverflowException"

printfn "\nDivision by zero count"   
let _ = Check.Quick div0