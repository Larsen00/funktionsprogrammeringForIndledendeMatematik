#r "../bin/Release/net7.0/main.dll"
#r "nuget: FsCheck"
open FsCheck
open Expression
open Number
open Generators
open SymbolicManipolation
open Differentiation
Arb.register<SmallEnvGen>()
Arb.register<NumberGen>()

// Skaleringsreglen
let scaleP f a env dx =
    eval (diff (a * f) dx) env = eval (a * diff f dx) env

// Sumreglen
let additionP f g env dx =
    eval (diff (f + g) dx) env = eval (diff f dx + diff g dx) env

// Subtraktionsreglen
let subtarctionP f g env dx =
    eval (diff (f - g) dx) env = eval (diff f dx - diff g dx) env

// Produktreglen
let multiplicationP f g env dx =
    eval (diff (f * g) dx) env = eval (diff f dx * g + f * diff g dx) env

// Kvotientreglen
let divisionP f g env dx =
    eval (diff (f / g) dx) env = eval ((g * diff f dx - f * diff g dx) / (g * g)) env

// Property test of differentiation 
let diffPBT ((env ,xlist):SmallEnv) (a:Number)= 
    let result = 
        try
            let a = N a
            let dx = if xlist <> [] then List.head xlist else 'X'
            let exprList = Gen.sample 1 2 (exprGen xlist 10 leafGen) 
            let e1::[e2] = exprList
            let e1 = simplifyExpr e1
            let e2 = simplifyExpr e2
            if 
                scaleP e1 a env dx && scaleP e2 a env dx && 
                additionP e1 e2 env dx &&
                subtarctionP e1 e2 env dx &&
                multiplicationP e1 e2 env dx &&
                divisionP e1 e2 env dx
            then 1
            else 0
        with
        | :? System.DivideByZeroException as _ -> 2
        | :? System.OverflowException as _ -> 3
    (result = 1 || result = 2 || result = 3)
    |> Prop.classify (result = 1) "Propertys Holds"
    |> Prop.classify (result = 2) "DivideByZeroExceptions"
    |> Prop.classify (result = 3) "OverflowException"

printfn "Differentiation property based testing"
let _ = Check.Quick diffPBT