#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open Matrix
open Number


let v1 = vector [Int 1; Int -1; Int 1]
let v2 = vector [Int 1; Int 0; Int 1]
let v3 = vector [Int 1; Int 1; Int 2]

let m1 = matrix [v1; v2; v3]

printfn "%A" (Gram_Schmidt m1 (fun _ -> (M([], C))))

