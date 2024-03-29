#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
#r "nuget: FsCheck"
open FsCheck
open Generators


Arb.register<MaxtrixGen>()
Arb.register<VectorGen>()
Arb.register<NumberGen>()


// Test the associativity property for vector addition Theorem 7.2 - 1 & 2
let _ = Check.Quick vectorAss

// Test the associativity property for vector addition Theorem 7.2 - 3
let _ = Check.Quick vectorScalarAss

let _ = Check.Quick vectorAssCom