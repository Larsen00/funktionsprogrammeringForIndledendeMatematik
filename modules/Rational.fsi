
module rational
[<Sealed>]
type rational =
    static member ( ~- ) : rational -> rational
    static member ( + )  : rational * rational -> rational
    static member ( + )  : int * rational -> rational
    static member ( - )  : rational * rational -> rational
    static member ( - )  : int * rational -> rational
    static member ( * )  : int  * rational -> rational
    static member ( * )  : rational * int -> rational
    static member ( * )  : rational * rational -> rational
    static member ( / )  : rational * rational -> rational
    static member ( / )  : int * rational -> rational
    static member ( / )  : rational * int -> rational


val make : int * int -> rational
val equal : rational * rational -> bool
val posetive : rational -> bool
val toString : rational -> string
val isZero : rational -> bool
val isOne : rational -> bool
val isInt : rational -> bool
val makeRatInt : rational -> int
val greaterThan  : rational * rational -> bool