
#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open Rational

#r "nuget: FsCheck"
open FsCheck

type Number = 
            | Re of Rational
            | Cn of Rational * Rational



let rec stringNumber n = 
    match n with
    | Cn (a, b) when posetive(b) -> help a b " +"
    | Cn (a, b) -> help a b " "
    | Re a ->  toString(a)

and help  a b sign =
    toString(a) + sign + toString(b)


// multiplication of Complex number with reel number
let mulRational z r =  
    match z with
    | Cn (a, b) -> Cn (a * r, b * r)
    | Re a -> Re (a * r)


// addision of two complex numbers - Definition 3.3
let rec add x y = 
    match (x, y) with
    | (Cn (a, b), Cn (c, d)) -> Cn (a + c, b + d)
    | (Re a, Re b) -> Re (a + b)
    | (Re a, _) -> add (Cn (a, make(0, 0))) y
    | (_, Re a) -> add (Cn (a, make(0, 0))) x
   

// subtraction of two complex numbers - Definition 3.3
let sub z1 z2 = 
    match z2 with
    | Re a -> add z1 (Re (-a))
    | Cn (a, b) -> add z1 (Cn (-a, -b))

// multiplication of two complex numbers - Definition 3.5
let mul x y =
    match x, y with
    | Cn (a, b), Cn (c, d) -> Cn (a*c-b*d, b*c+a*d)
    | Re a, Re b -> Re (a * b)
    | Re a, z -> mulRational z a
    | z , Re a ->  mulRational z a

// complex conjugation - Definition 3.8
let conjugate n = 
    match n with
    | Cn (a, b) -> Cn (a, -b)
    | _ -> n
    

// Division of complex numbers with real numbers
let divRational n r = 
    match n with
    | _ when isZero(r) -> failwith "zero division error"
    | Re a -> Re (a / r) 
    | Cn (a, b) -> Cn (a / r, b / r) 

// Multiplying a complex number with its conjugation
let mulConjugate n =
    match n with 
    | Cn (a, b) -> Re (a*a + b*b)
    | Re a -> Re (a * a) 

// Division of complex numbers
let divComplex z1 z2 =
    let (Re denominator) = mulConjugate z2
    divRational (mul z1 (conjugate z2)) denominator


// Comparing two Numbers
let compareNumbers a b =
    match a, b with
    | Re a, Re b -> equal(a, b)
    | Cn (a, b), Cn (c, d) ->  equal(a, c) && equal(b, d)
    | _, _ -> false


// Property bases testing of Theorem 3.10
// Commutative
let commutativeTest f = 
    fun (z1: Number) (z2: Number) ->
    let result1 = f z1 z2
    let result2 = f z2 z1
    compareNumbers result1 result2

// Associative
let associativeTest f = 
    fun (z1: Number) (z2: Number) (z3: Number) -> 
    let result1 = f z1 (f z2 z3)
    let result2 = f (f z1 z2) z3
    compareNumbers result1 result2

// Distributivity 
let distributivity (z1: Number) (z2: Number) (z3: Number) = 
    let result1 = mul z1 (add z2 z3)
    let result2 = add (mul z1 z2) (mul z1 z3)
    compareNumbers result1 result2

// test
let _ = Check.Quick (commutativeTest add)
let _ = Check.Quick (commutativeTest mul)
let _ = Check.Quick (associativeTest add)
let _ = Check.Quick (associativeTest mul)
let _ = Check.Quick distributivity


// Algorithm 2 for computing the “multiplicative inverse of z - Example 3.12
let multiplicativeInverse z =
 match z with
 | Cn(c, d) when isZero(c) && isZero(d) -> failwith "“0 has no multiplicative inverse!"
 | Re (a) -> Re(1/a)
 | Cn (c, d) -> 
            let N = c*c + d*d
            Cn (c/N, -d/N)




(*
// TEST
printfn "\nTEST"
let Z1 = Cn (1.0, 2.0)
let Z2 = Cn (3.0, 4.0)
printfn "%A" (stringNumber Z1)
printfn "%A" (stringNumber (mul Z1 Z2))
printfn "%A" (stringNumber (divComplex Z1 Z2))
*)
