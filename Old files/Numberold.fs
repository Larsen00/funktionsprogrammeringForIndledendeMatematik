module Number
open rational
open complex


type Number = 
        | Int of int 
        | Rational of rational
        | Complex of complex<Number>


// returns the specific number 
let zero = Int 0
let one  = Int 1
let two  = Int 2


// #TODO med tiden vil man kunne tilføje funktionalitet sådan at man kan mixe int rationalle tal her ville man lave en make complex måske. og en try make rational.

// Creates a rational number from an Number
let makeRational a =
    match a with
    | Int x       -> make(x, 1)
    | Rational x  -> x
    | Complex _   -> failwith "makeRational: Cannot convert complex number to rational"

let makeComplex a =
    match a with
    | Int x       -> Cn(Int x , zero)
    | Rational x  -> Cn(Rational x, zero)
    | Complex x   -> x

// binary operation on two numbers
// let operation a b f =
//     f (makeRational a) (makeRational b) |> Rational

let operation a b f = 
    f (makeComplex a) (makeComplex b) |> Complex

// negates a number
let rec neg a =
    match a with
    | Int a -> Int -a
    | Rational a -> Rational -a
    | Complex (Cn(a, b)) -> Complex (Cn(neg a, neg b))

// checks if a number is zero
let rec isZero n =
    match n with
    | Int a -> a = 0
    | Rational a -> rational.isZero(a)
    | Complex(Cn(a, b)) -> isZero a && isZero b


// comaress if a > b
// only used to avoid negative numbers after subtraction, hence the complex implementation is not needed
let rec compare a b =
    match a, b with
    | Int x, Int y -> x > y
    | Rational x, Rational y -> greaterThan(x, y)
    | Int x, Rational y | Rational y, Int x -> greaterThan(make(x, 1), y)
    | Complex (Cn(a, b)) , Complex (Cn(c, d)) -> compare a c && compare b d 
    | _, Complex _ | Complex _ , _ -> false

// checks rational for being integer #TODO complex numer skal også tjekkes
let tryMakeInt r =
    match r with
    | Rational a when isInt a -> Int (makeRatInt a)
    | _ -> r

// Reduces a number to the smalleest subset of numbers
let rec tryReduceNumberSet n =
    match n with
    | Complex(Cn(a, b)) when isZero b -> tryReduceNumberSet a
    | Complex(Cn(a, b)) -> Complex(Cn(tryReduceNumberSet a, tryReduceNumberSet b))
    | Rational a when isInt a -> Int (makeRatInt a)
    | _ -> n


type Number with
    static member (+)  (a, b) = operation a b (+) |> tryMakeInt
    static member (-)  (a, b) = operation a b (-) |> tryMakeInt
    static member (*)  (a, b) = operation a b (*) |> tryMakeInt
    static member (/)  (a, b) = operation a b (/) |> tryMakeInt
    static member (~-) (a)    = neg a |> tryMakeInt




// compares two numbers a >b
let greaterThan a b = compare a b



// checks if a number is one
let rec isOne n = 
    match n with 
    | Int a -> a = 1
    | Rational a -> rational.isOne(a)
    | Complex(Cn(a, b)) -> isOne a && isZero b

// returns the string representation of a number
let rec toString n = 
    match n with
    | Int a -> sprintf "%d" a
    | Rational a -> rational.toString a
    | Complex(Cn(a, b)) -> toString a + " + " + toString b + "I"


// Determine is a number is negative, is used to negate the number hence why reel and imaginary part both need to be negative
let rec isNegative n = 
    match n with
    | Int a -> a < 0
    | Rational a -> rational.isNegative a
    | Complex(Cn(a, b)) -> isNegative a && isNegative b

// returns the absolute value of a number
let rec absNumber n = 
    match n with
    | Int a -> Int (abs a)
    | Rational a -> Rational (absRational(a))
    | Complex(Cn(a, b)) -> Complex(Cn(absNumber a, absNumber b))









