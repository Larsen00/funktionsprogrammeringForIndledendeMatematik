#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open Number
open Matrix
open rational
open complex
open Expression

let v1 = vector [Int 3; Int 0; Int 0; Int 0]
let v2 = vector [Int 1; Int 2; Int 0; Int 1]
let v3 = vector [Int 1; Int 1; Int 1; Int 2]

let m1 = matrix [v1; v2; v3]
stringMatrix m1 |> printfn "%A"

let b = vector [Int 1; Int 0; Int 2; Int 3]

matrixEquation m1 b |> printfn "%A"

// let m = M ([V ([Int 0; Int 0; Rational (R (-1, 3)); Int 1], Matrix.R);
//             V ([Int -2; Complex (C (R (5, 1), R (6, 1))); Int 0;
//                 Complex (C (R (-3, 1), R (-1, 1)))],  Matrix.R);
//             V ([Int 0; Int 0; Int 1; Int 0],  Matrix.R);
//             V ([Int -1; Complex (C (R (2, 1), R (3, 1))); Rational (R (8, 3)); Int 1],  Matrix.R)],
//          Matrix.R)


// printfn "%A" (stringMatrix <| rowEchelonForm m)