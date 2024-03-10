#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
#r "nuget: FsCheck"
#load "Generators.fsx"
#load "../modules/Vector.fs"
open FsCheck
open rantionalAndComplex
open Vector
open Number
open Generators



// Property: The addition of vectors is associative Theorem 7.2
let vectorAss (M m) =
     


// let _ = Check.Quick vectorAss

