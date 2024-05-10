module complex
open rational

type complex = | C of rational * rational
with
    static member ( + )  : complex * complex -> complex
    static member ( - )  : complex * complex -> complex
    static member ( * )  : rational * complex -> complex
    static member ( * )  : complex * rational -> complex
    static member ( * )  : complex * complex -> complex
    static member ( / )  : complex * rational -> complex
    static member ( / )  : complex * complex -> complex
    static member ( ~- ) : complex -> complex

val newComplex : rational * rational -> complex
val isGreater : complex * complex -> bool
val realPart : complex -> rational
val isReal : complex -> bool
val isZero : complex -> bool
val toString : complex -> string
val isNegative : complex -> bool
val absComplex : complex -> complex
val conjugate : complex -> complex