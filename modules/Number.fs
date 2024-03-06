module Number
open rational
type Number = | Int of int | Rational of rational

// #TODO med tiden vil man kunne tilføje funktionalitet sådan at man kan mixe int rationalle tal her ville man lave en make complex måske. og en try make rational.

// Creates a rational number from an Number
let makeRational a =
    match a with
    | Int x       -> make(x, 1)
    | Rational x  -> x

// binary operation on two numbers
let operation a b f =
    f (makeRational a) (makeRational b) |> Rational


// negates a number
let neg a =
    // printfn "neg - %A" a
    match a with
    | Int a -> Int -a
    | Rational a -> Rational -a


let compare a b =
    match a, b with
    | Int x, Int y -> x > y
    | Rational x, Rational y -> greaterThan(x, y)
    | Int x, Rational y | Rational y, Int x -> greaterThan(make(x, 1), y)

// checks rational for being integer #TODO complex numer skal også tjekkes
let tryMakeInt r =
    match r with
    | Rational a when isInt a -> Int (makeRatInt a)
    | _ -> r
    
type Number with
    static member (+)  (a, b) = operation a b (+) |> tryMakeInt
    static member (-)  (a, b) = operation a b (-) |> tryMakeInt
    static member (*)  (a, b) = operation a b (*) |> tryMakeInt
    static member (/)  (a, b) = operation a b (/) |> tryMakeInt
    static member (~-) (a)    = neg a |> tryMakeInt


// compares two numbers a >b
let greaterThan a b = compare a b

// checks if a number is zero
let isZero n =
    match n with
    | Int a -> a = 0
    | Rational a -> isZero(a)


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
let isNegative n = 
    match n with
    | Int a -> a < 0
    | Rational a -> rational.isNegative a

let abs n = 
    match n with
    | Int a -> Int (abs a)
    | Rational a -> Rational (absRational(a))