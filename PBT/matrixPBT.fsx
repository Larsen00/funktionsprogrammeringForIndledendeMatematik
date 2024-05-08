#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
#r "nuget: FsCheck"
open FsCheck
open Generators


Arb.register<MaxtrixGen>()
Arb.register<NumberGen>()


// Test the associativity property for vector addition Theorem 7.2 - 1 & 2
let _ = Check.Quick vectorCom

// Test the associativity property for vector addition Theorem 7.2 - 3
let _ = Check.Quick vectorScalarAss

let _ = Check.Quick vectorAssCom
let _ = Check.Quick transposeTwice

Arb.register<IndependetBacisGen>()
printfn "gramSchmidtIsOrthogonal 1"
let _ = Check.Quick gramSchmidtIsOrthogonal

Arb.register<fullRankedMatrixGen>()
printfn "gramSchmidtIsOrthogonal 2"
let _ = Check.Quick gramSchmidtIsOrthogonal2

printfn "fullRankedMatrixIsFullRanked"
let _ = Check.Quick fullRankedMatrixIsFullRanked

(*
#TODO
fullRankedMatrixIsFullRanked
Falsifiable, after 66 tests (0 shrinks) (StdGen (1421224008, 297328343)):
Original:
M ([V ([Int 0; Int 0; Rational (R (-1, 3)); Int 1], R);
    V ([Int -2; Complex (C (R (5, 1), R (6, 1))); Int 0;
        Complex (C (R (-3, 1), R (-1, 1)))], R);
    V ([Int 0; Int 0; Int 1; Int 0], R);
    V ([Int -1; Complex (C (R (2, 1), R (3, 1))); Rational (R (8, 3)); Int 1], R)],
   R)
*)
