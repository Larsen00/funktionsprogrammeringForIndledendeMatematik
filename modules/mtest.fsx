#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open Matrix
open Number
open rational

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


let n = 5
let i = 2
let j = 3
// printfn "\n%A" (stringMatrix (m1*m1) )
// printfn "\n%A" (stringMatrix <| standardBacis 5)
printfn "\n%A" ( rowOperation i j (Int 2) <| standardBacis n |> stringMatrix )
printfn "\n%A" ( standardBacis n |> rowOperation i j (Int 2) |> rowEchelonForm |> stringMatrix )
printfn "\n%A" (  standardBacis n |> rowOperation i j (Int 2) |> rowOperation i j (Int -2) |> stringMatrix )