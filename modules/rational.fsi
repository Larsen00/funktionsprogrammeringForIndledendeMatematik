module rational

type rational = R of int * int 
    with
    static member ( ~- ) : rational -> rational
    static member ( + )  : rational * rational -> rational
    static member ( + )  : int * rational -> rational
    static member ( - )  : rational * rational -> rational
    static member ( - )  : int * rational -> rational
    static member ( * )  : int * rational -> rational
    static member ( * )  : rational * int -> rational
    static member ( * )  : rational * rational -> rational
    static member ( / )  : rational * rational -> rational
    static member ( / )  : int * rational -> rational
    static member ( / )  : rational * int -> rational
    static member ( / )  : int * int -> rational

val newRational  : int * int -> rational
val equal        : rational * rational -> bool
val positive     : rational -> bool
val toString     : rational -> string
val isZero       : rational -> bool
val isOne        : rational -> bool
val isInt        : rational -> bool
val makeInt      : rational -> int
val greaterThan  : rational * rational -> bool
val isNegative   : rational -> bool
val absRational  : rational -> rational
