module complex

open rational
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
        sprintf "%s + %si" (toStringR a) (toStringR b)
    else 
        sprintf "%s - %si" (toStringR a) (absRational b |> toStringR)
let absComplex (C(a, b)) = C(absRational(a), absRational(b))
