module Number

[<Sealed>]
type Number =
    static member ( + )  : Number * Number -> Number
    static member ( - )  : Number * Number -> Number 
    static member ( * )  : Number * Number -> Number 
    static member ( ~- ) : Number -> Number
    static member ( / )  : Number * Number -> Number

    
val isZero : Number -> bool
val getZero : Number -> Number
val zero : Number
val isOne : Number -> bool
val one : Number 
val two : Number
val toString : Number -> string
val greaterThan : Number -> Number -> bool
val tryMakeInt : Number -> Number
val isNegative : Number -> bool
val abs : Number -> Number