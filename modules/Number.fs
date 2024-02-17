module Number
open Rational
type Number = | Int of int | Rational of Rational

// med tiden vil man kunne tilføje funktionalitet sådan at man kan mixe int rationalle tal

let add a b =
    match a, b with
    | Int a, Int b -> Int (a + b)
    | Rational a, Rational b -> Rational (a + b)
    | _, _ -> failwith "failed in Number_add"

let sub a b =
    match a, b with
    | Int a, Int b -> Int (a - b)
    | Rational a, Rational b -> Rational (a - b)
    | _, _ -> failwith "failled in Number sub"

let mul a b =
    match a, b with
    | Int a, Int b -> Int (a*b)
    | Rational a, Rational b -> Rational (a*b)
    | _, _ -> failwith "failled in Number mul"

type Number with
    static member (+)  (a, b)       = add a b
    static member (-)  (a, b)       = sub a b
    static member (*)  (a, b)       = mul a b 


let isZero n =
    match n with
    | Int a -> a = 0
    | Rational a -> isZero(a)

let getZero n = 
    match n with
    | Int _ -> Int 0
    | Rational _ -> Rational (make(0, 1))

let zero = Int 0

let isOne n = 
    match n with 
    | Int a -> a = 1
    | Rational a -> isOne(a)

let two = Int 2
let one = Int 1
let toString n = 
    match n with
    | Int a -> sprintf "%d" a
    | Rational a -> toString a