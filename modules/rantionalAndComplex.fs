module rantionalAndComplex



type rational = R of int * int

// determines if a rational number is zero
let isZeroR(R(a, b)) = a = 0 || b = 0

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
    if isZeroR(R(p, q)) then R(0, 1)
    else
        let ap = abs p
        let aq = abs q
        let sign = if (p/ap * q/aq) > 0 then 1 else -1
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
let toStringR(R(a,b)) = sprintf "%d/%d" a b


// determines if a rational number is one
let isOne(R(a, b)) = not (isZeroR(R(a, b))) && a = b

// determines if a rational number is an integer
let isInt(R(_, b)) = b = 1

// returns the integer value of a rational number
let makeRatInt(R(a, b)) = if isInt(R(a, b)) then a else failwith "Not an integer"

// constructor of a rational number
let makeR(x, y) =
    if y <> 0 then
        mkQ(R(x, y))
    else
         raise (System.DivideByZeroException("rational.make: Cannot divide by zero!"))

// checks if a rational number is negative
let isNegativeR(R(a, b)) = a*b < 0

// takes the absolute value of a rational number
let absRational(R(a, b)) = R(abs a, abs b)



type complex = | C of rational * rational

let makeC (a, b) = C(a, b)

// division of a complex number with a number
let complexDivNumber c (n) = 
    match c with
    | _ when isZeroR n ->  raise (System.DivideByZeroException("Complex.divRational: Cannot divide by zero!"))
    | C (a, b) -> C (a / n, b / n) 

// Multiplying a number with its conjugation a*a + b*b
let mulConjugate (C(a, b)) = a*a + b*b

// complex conjugation - Definition 3.8 
let conjugate (C (a, b)) = C (a, -b)

// multiplication of complex numbers
let mulComplex (C (a, b)) (C (c, d)) = C(a*c-b*d, b*c+a*d)

// Division of complex numbers
let divComplex z1 z2 =
    let n = mulConjugate z2
    complexDivNumber (mulComplex z1 (conjugate z2)) n

type complex with
    static member (+)  (C(a, b), C(c, d)) =  C(a + c, b + d)
    static member (-)  (C(a, b), C(c, d)) =  C(a - c, b - d)
    static member (*)  (n, C(a, b))       =  C(n * a, n * b)
    static member (*)  (C(a, b), n)       =  C(n * a, n * b)
    static member (*)  (z1, z2)           =  mulComplex z1 z2
    static member (/)  (z, n)             =  complexDivNumber z n
    static member (/)  (z1, z2)           =  divComplex z1 z2 
    static member (~-) (C(a, b))          =  C(-a, -b)

let isGreater (C(a,b), C(c, d)) = greaterThan(a, c) && greaterThan(b, d)
let realPart (C(a, _)) = a
let isReal (C(_, b)) = isZeroR b
let isZeroC (C(a, b)) = isZeroR a && isZeroR b
let isNegativeC (C(a, b)) = isNegativeR a && isNegativeR b
let toStringC (C(a, b)) = 
    if isNegativeR b then
        sprintf "%s +%si" (toStringR a) (toStringR b)
    else 
        sprintf "%s %si" (toStringR a) (toStringR b)
let absComplex (C(a, b)) = C(absRational(a), absRational(b))
