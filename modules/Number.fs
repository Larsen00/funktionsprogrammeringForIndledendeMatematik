module Number
open rational
open complex

type Number = 
        | Int of int
        | Rational of rational
        | Complex of complex
        

// Creates a rational number from an Number
let makeRational a =
    match a with
    | Int x       -> newRational(x, 1)
    | Rational x  -> x
    | Complex _   -> raise (System.NotSupportedException("Cannot convert complex to rational"))

let makeComplex n =
    match n with
    | Int x -> newComplex (newRational(x, 1), newRational(0, 1))
    | Rational x -> newComplex (x, newRational(0, 1))
    | Complex x -> x

// binary operation on two numbers
let operation a b f =
    f (makeComplex a) (makeComplex b) |> Complex


// negates a number
let neg a =
    match a with
    | Int a -> Int -a
    | Rational a -> Rational -a
    | Complex a -> Complex -a


// check if a > b
// only used to avoid negative numbers after subtraction, hence the complex implementation is not needed
let rec compare a b =
    match a, b with
    | Int x, Int y           -> x > y
    | Int x, Rational y      -> greaterThan (newRational (x, 1), y)
    | Complex x, Complex y   -> isGreater (x, y)
    | Rational x, Rational y -> greaterThan (x, y)
    | Int x, Complex y       -> isGreater (newComplex (newRational(x, 1), newRational(0, 1)), y)
    | Rational x, Complex y  -> isGreater (newComplex (x, newRational(0, 1)), y)
    | _, _                   -> not (compare b a)

let rec tryReduce n =
    match n with
    | Complex a when isReal a -> realPart a |> Rational |> tryReduce
    | Rational a when isInt a -> makeInt a |> Int
    | _ -> n

    
type Number with
    static member (+)  (a, b) = operation a b (+) |> tryReduce
    static member (-)  (a, b) = operation a b (-) |> tryReduce
    static member (*)  (a, b) = operation a b (*) |> tryReduce
    static member (/)  (a, b) = operation a b (/) |> tryReduce
    static member (~-) (a)    = neg a |> tryReduce


// compares two numbers a >b
let greaterThan a b = compare a b

// checks if a number is zero
let isZero n =
    match n with
    | Int a -> a = 0
    | Rational a -> rational.isZero(a)
    | Complex a -> complex.isZero(a)


// checks if a number is one
let isOne n = 
    match n with 
    | Int a -> a = 1
    | Rational a -> isOne(a)
    | Complex a -> isReal a && realPart a |> isOne

// returns the string representation of a number
let toString n = 
    match n with
    | Int a -> sprintf "%d" a
    | Rational a -> rational.toString a
    | Complex a -> complex.toString a

// returns the specific number 
let zero = Int 0
let one = Int 1
let two = Int 2

// checks if a number is negative
let isNegative n = 
    match n with
    | Int a -> a < 0
    | Rational a -> rational.isNegative a
    | Complex a -> complex.isNegative a

// is used to negate a number, hence why both the real and imaginary part needs to be negative
let absNumber n = 
    match n with
    | Int a -> Int (abs a)
    | Rational a -> Rational (absRational(a))
    | Complex a -> Complex (absComplex(a))

let conjugate n = 
    match n with
    | Complex a -> conjugate(a) |> Complex
    | _ -> n

let inv n =
    one / n

let isInt n = 
    match n with
    | Int _ -> true
    | _ -> false