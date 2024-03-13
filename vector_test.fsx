
#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"

#load "modules/Number.fs"
#load "modules/Matrix.fs"
open Number
open Matrix

let n = Int 2
let n2 = Int 3

let v1 = V ([n; n2], C)
let v2 = V ([n2; n], C)
let v3 = V ([n; n2], R)

let m1 = M ([v1; v2], C)
let m2 = M ([v3; v3], R)

printfn "%A" (m1 + m2) 

