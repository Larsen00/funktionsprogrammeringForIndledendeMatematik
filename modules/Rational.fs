module rational
type rational = R of int * int

// determines if a rational number is zero
let isZero(R(a, b)) = a = 0 || b = 0

// checks if a rational number is posetive
let posetive(R(a,b)) = a*b > 0

// returns greatest common divisor
// #TODO: make faster version
let rec gcd r =
    // printfn "gcd - %A" r
    match r with
    | a, b when a < 0 || b <  0-> failwith "Euclid's algorithm dosen't allow negative numbers"
    | a, b when a = b -> a
    | a, b when a > b -> gcd(a-b, b)
    | a, b -> gcd(a, b - a) 

// Functional Programming Using F# page 57
// reduces a rational number to its simplest form
let canc(R(p, q)) =
    // printfn "canc - %A %A" p q
    if 
        isZero(R(p, q)) then R(0, 1)
    else
        let sign = if posetive(R(p, q)) then 1 else -1
        let ap = abs p
        let aq = abs q
        let d = gcd (ap, aq)
        R(sign * (ap / d), aq / d)

// checks for division by zero before reducing a rational number
let mkQ = function
    | R(_, 0) ->  raise (System.DivideByZeroException("rational.mkQ: Cannot divide by zero!"))
    | r -> canc r

type rational with
    static member (~-) (R(x,y))             = canc(R(-x, y))
    static member (+)  (R(a,b),R(c,d))      = canc(R(a*d+b*c, b*d))
    static member (+)  (a,R(c,d))           = canc(R(a*d+c, d))
    static member (-)  (R(a,b),R(c,d))      = canc(R(a*d-b*c, b*d))
    static member (-)  (a,R(c,d))           = canc(R(a*d-c, d))
    static member (*)  (a, R(x,y))          = canc(R(a*x, a*y))
    static member (*)  (R(x,y), a)          = canc(R(a*x, a*y))
    static member (*)  (R(a,b),R(c,d))      = canc(R(a*c, b*d))
    static member (/)  (a, R(x,y))          = mkQ(R(a*y, x))
    static member (/)  (R(x,y), a)          = mkQ(R(x, a*y))
    static member (/)  (R(a,b),R(c,d))      = mkQ(R(a*d, b*c))
    static member (/)  (a,b)                = mkQ(R(a, b))



// checks if two rational numbers are equal
let equal(R(a,b), R(c,d))  = (a*d = c*b)

// checks if a rational number is greater than another
let greaterThan(R(a,b),R(c,d)) = a*d > c*b

// returns the string representation of a rational number
let toString(R(a,b)) = sprintf "%d/%d" a b


// determines if a rational number is one
let isOne(R(a, b)) = not (isZero(R(a, b))) && a = b

// determines if a rational number is an integer
let isInt(R(_, b)) = b = 1

// returns the integer value of a rational number
let makeRatInt(R(a, b)) = if isInt(R(a, b)) then a else failwith "Not an integer"

// constructor of a rational number
let make(x, y) =
    if y <> 0 then
        mkQ(R(x, y))
    else
         raise (System.DivideByZeroException("rational.make: Cannot divide by zero!"))

// checks if a rational number is negative
let isNegative(R(a, b)) = a*b < 0

// takes the absolute value of a rational number
let absRational(R(a, b)) = R(abs a, abs b)