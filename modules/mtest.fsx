#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open Matrix
open Number
open rational

let v1 = vector [Int 3; Int 0; Int 0]
let v2 = vector [Int 1; Int 2; Int 0]
let v3 = vector [Int 1; Int 1; Int 1]

let m1 = matrix [v1; v2; v3]
stringMatrix m1 |> printfn "%A"

let b = vector [Int 1; Int 0; Int 2]

Axequalb m1 b
