module Number

[<Sealed>]
type Number =
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
val tryReduce   : Number -> Number
val toString     : Number -> string
