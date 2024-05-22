#r "../../bin/Release/net7.0/main.dll"
open MyInt
let a = I 3
factorial a |> factorial |> printfn "%A"

