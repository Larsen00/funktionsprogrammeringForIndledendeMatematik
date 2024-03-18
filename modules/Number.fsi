module Number
// open rantionalAndComplex
open rational
open complex

type Number =
    | Int of int
    | Rational of rational
    | Complex of complex
    with
    static member ( + )  : Number * Number -> Number
    static member ( - )  : Number * Number -> Number 
    static member ( * )  : Number * Number -> Number 
    static member ( / )  : Number * Number -> Number
    static member ( ~- ) : Number -> Number




val zero         : Number
val one          : Number 
val two          : Number
val isZero       : Number -> bool
val isOne        : Number -> bool
val isNegative   : Number -> bool
val absNumber    : Number -> Number
val greaterThan  : Number -> Number -> bool
val tryReduce    : Number -> Number
val toString     : Number -> string
val conjugate    : Number -> Number
val inv          : Number -> Number