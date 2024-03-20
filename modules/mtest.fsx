#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open Matrix
open Number


let v1 = vector [Int 1; Int -1; Int 1]
let v2 = vector [Int 1; Int 0; Int 1]
let v3 = vector [Int 1; Int 1; Int 2]

// let v1 = vector [Int 1; Int 1]
// let v2 = v1
// let v3 = vector [Int 1; Int 0]


let m1 = matrix [v1; v2; v3]

// printfn "%A" (orthogonalBacis m1)


 
printfn "%A\n" (stringMatrix m1)
let rf = (rowEchelonForm m1)
printfn "res:\n%A" (stringMatrix rf)



printfn "%A" (stringMatrix (m1*m1) )