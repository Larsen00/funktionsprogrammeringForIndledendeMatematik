#r "../bin/Release/net7.0/main.dll"
#r "nuget: FsCheck"
open FsCheck
open Number
open Generators

Arb.register<NumberGen>()

// Addition og multiplikation er associative
let associative (a:Number) (b:Number) (c:Number) = 
    a + (b + c) = (a + b) + c && a * (b * c) = (a * b) * c

// Addition og multiplikation er kommutative
let commutative (a:Number) (b:Number) = 
    a + b = b + a && a * b = b * a

// Distributivitet af multiplikation over addition
let distributive (a:Number) (b:Number) (c:Number) = 
    a * (b + c) = a * b + a * c

// Addition og multiplikation har et neutralt element
let neutralAdditive (a:Number) = 
    a + zero = a && a * one = a

// Omvendt funktion eksisterer til addition
let inverseAdditive (a:Number) = 
    a + (-a) = zero

// Omvendt funktion eksisterer til multiplikation
let inverseMultiplicative (a:Number) = 
    let res = 
        try 
            if a * inv a = one then 1 else 0
        with 
            | :? System.DivideByZeroException -> 2
    (res = 1 || res = 2)
    |> Prop.classify (res = 1) "PropertyHolds"
    |> Prop.classify (res = 2) "DivideByZeroExceptions"

let _ = Check.Quick associative
let _ = Check.Quick commutative
let _ = Check.Quick distributive
let _ = Check.Quick neutralAdditive
let _ = Check.Quick inverseAdditive
let _ = Check.Quick inverseMultiplicative


