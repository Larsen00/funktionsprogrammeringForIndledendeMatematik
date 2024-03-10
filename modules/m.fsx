
#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"

#load "Number.fs"
#load "Vector.fs"
open Number
open Vector

let n = Int 2
let n2 = Int 3

let v1 = V [n; n2]
let v2 = V [n2; n]

printfn "%A" (v1 + v2)


