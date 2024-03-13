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

val makeR         : int * int -> rational
val equal        : rational * rational -> bool
val posetive     : rational -> bool
val toStringR     : rational -> string
val isZeroR       : rational -> bool
val isOne        : rational -> bool
val isInt        : rational -> bool
val makeRatInt   : rational -> int
val greaterThan  : rational * rational -> bool
val isNegativeR   : rational -> bool
val absRational  : rational -> rational
