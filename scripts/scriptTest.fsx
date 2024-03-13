#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open TreeGenerator
open Differentiation
open Expression
open Number
open SymbolicManipolation

let ligning = simplifyExpr (tree "(3*x-5)*2")
printfn "Orginal equation: %A" (InfixExpression ligning) 

let dx = (diff ligning 'x') |> simplifyExpr
printfn "Differentiated equation: %A" (InfixExpression dx)

