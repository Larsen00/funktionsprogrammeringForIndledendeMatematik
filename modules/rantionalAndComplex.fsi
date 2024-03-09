module rantionalAndComplex

[<Sealed>]
type rational =
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


[<Sealed>]
type complex =
    static member ( + )  : complex * complex -> complex
    static member ( - )  : complex * complex -> complex
    static member ( * )  : rational * complex -> complex
    static member ( * )  : complex * rational -> complex
    static member ( * )  : complex * complex -> complex
    static member ( / )  : complex * rational -> complex
    static member ( / )  : complex * complex -> complex
    static member ( ~- ) : complex -> complex

val makeC : rational * rational -> complex
val isGreater : complex * complex -> bool
val realPart : complex -> rational
val isReal : complex -> bool
val isZeroC : complex -> bool
val toStringC : complex -> string
val isNegativeC : complex -> bool
val absComplex : complex -> complex