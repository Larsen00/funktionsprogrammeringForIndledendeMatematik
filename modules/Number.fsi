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
val abs          : Number -> Number
val greaterThan  : Number -> Number -> bool
val tryMakeInt   : Number -> Number
val toString     : Number -> string
