module Number
open Rational
type Number = | Int of int | Rational of Rational

// #TODO med tiden vil man kunne tilføje funktionalitet sådan at man kan mixe int rationalle tal

// mathemathical operations on two numbers
let operation a b f g =
  match a, b with
  | Int x, Int y -> Int (f x y)
  | Rational x, Rational y -> Rational (g x y)
  | Int x, Rational y | Rational y, Int x -> Rational (g (make(x, 1)) y)

// negates a number
let neg a =
    match a with
    | Int a -> Int -a
    | Rational a -> Rational -a

// checks rational for being integer #TODO complex numer skal også tjekkes
let tryMakeInt r =
    match r with
    | Rational a when isInt a -> Int (makeRatInt a)
    | _ -> r

type Number with
    static member (+)  (a, b)       = operation a b (+) (+)
    static member (-)  (a, b)       = operation a b (-) (-)
    static member (*)  (a, b)       = operation a b ( * ) ( * )
    static member (~-) (a)          = neg a 
    static member (/)  (a, b)       = operation a b (/) (/)

// checks if a number is zero
let isZero n =
    match n with
    | Int a -> a = 0
    | Rational a -> isZero(a)

// returns a zero number
let getZero n = 
    match n with
    | Int _ -> Int 0
    | Rational _ -> Rational (make(0, 1))


// checks if a number is one
let isOne n = 
    match n with 
    | Int a -> a = 1
    | Rational a -> isOne(a)

// returns the string representation of a number
let toString n = 
    match n with
    | Int a -> sprintf "%d" a
    | Rational a -> toString a

// returns the specific number 
let zero = Int 0
let one = Int 1
let two = Int 2