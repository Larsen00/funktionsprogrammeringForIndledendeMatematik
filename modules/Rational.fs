module Rational
type Rational = R of int * int
let make(x, y) =
    if y <> 0 then
        R(x, y)
    else
        failwith "Denominator cannot be zero"
let posetive(R(a,b)) = a*b > 0
let rec gcd(r) = 
    match r with
    | a, b when a < 0 || b <  0-> failwith "Euclid's algorithm dosen't allow negative numbers"
    | a, b when a = b -> a
    | a, b when a > b -> gcd(a-b, b)
    | a, b -> gcd(a, b - a) 

// Functional Programming Using F# s57
let canc(R(p, q)) =
    let sign = if posetive(R(p, q)) then -1 else 1
    let ap = abs p
    let aq = abs q
    let d = gcd(ap, aq)
    R(sign * (ap / d), aq / d)

let mkQ = function
    | R(_, 0) -> failwith "Division by zero"
    | r -> canc r

type Rational with
    static member (~-) (R(x,y))             = canc(R(-x, y))
    static member (+)  (R(a,b),R(c,d))      = canc(R(a*d+b*c, b*d))
    static member (-)  (R(a,b),R(c,d))      = canc(R(a*d-b*c, b*d))
    static member (*)  (a, R(x,y))          = canc(R(a*x, a*y))
    static member (*)  (R(x,y), a)          = canc(R(a*x, a*y))
    static member (*)  (R(a,b),R(c,d))      = canc(R(a*c, b*d))
    static member (/)  (a, R(x,y))          = mkQ(R(a*y, x))
    static member (/)  (R(x,y), a)          = mkQ(R(x, a*y))
    static member (/)  (R(a,b),R(c,d))      = mkQ(R(a*d, b*c))
let equal(R(a,b), R(c,d))  = (a*d = c*b)
let toString(R(a,b)) = sprintf "%d/%d" a b
let isZero(R(a, b)) = a = 0 || b = 0
let isOne(R(a, b)) = not (isZero(R(a, b))) && a = b
