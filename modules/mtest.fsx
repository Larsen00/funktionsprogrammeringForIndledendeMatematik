#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open Number
open Matrix
open rational
open complex

// let v1 = vector [Int 3; Int 0; Int 0]
// let v2 = vector [Int 1; Int 2; Int 0]
// let v3 = vector [Int 1; Int 1; Int 1]

// let m1 = matrix [v1; v2; v3]
// stringMatrix m1 |> printfn "%A"

// let b = vector [Int 1; Int 0; Int 2]

// Axequalb m1 b

let m = M ([V ([Int 1; Int 0; Int 0; Int 0; Int 0; Int 0; Int 0], Matrix.R);
    V ([Int 2; Int 1; Int 0; Rational (R (2, 3)); Int 0; Int 0; Int 0], Matrix.R);
    V ([Int 0; Int 0; Int 1; Int 0; Int 0; Int 0; Int 0], Matrix.R);
    V ([Int 0; Complex (C (R (-5, 2), R (-1, 1))); Int 0;
        Complex (C (R (-2, 3), R (-2, 3))); Int 0; Int 0; Int 0], Matrix.R);
    V ([Int 0; Complex (C (R (-5, 3), R (-2, 3))); Int 0;
        Complex (C (R (-4, 9), R (-4, 9))); Int 1; Int 0; Int 0], Matrix.R)], Matrix.R)

let tm = transposeMatrix m
// printfn "%A" (stringMatrix tm)
let um = orthogonalBacis tm
// isOrthogonalBacis um |> printfn "%A"
hasSameSpan tm um |> printfn "hassamespan: %A"