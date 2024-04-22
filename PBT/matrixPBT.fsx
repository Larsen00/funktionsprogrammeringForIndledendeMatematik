#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
#r "nuget: FsCheck"
open FsCheck
open Generators


Arb.register<MaxtrixGen>()
Arb.register<NumberGen>()
Arb.register<IndependetBacisGen>()
Arb.register<fullRankedMatrixGen>()


// Test the associativity property for vector addition Theorem 7.2 - 1 & 2
let _ = Check.Quick vectorCom

// Test the associativity property for vector addition Theorem 7.2 - 3
let _ = Check.Quick vectorScalarAss

let _ = Check.Quick vectorAssCom
let _ = Check.Quick transposeTwice

let _ = Check.Quick gramSchmidtIsOrthogonal

let _ = Check.Quick gramSchmidtIsOrthogonal2
let _ = Check.Quick fullRankedMatrixIsFullRanked