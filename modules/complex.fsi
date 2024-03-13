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

val makeC : rational * rational -> complex
val isGreater : complex * complex -> bool
val realPart : complex -> rational
val isReal : complex -> bool
val isZeroC : complex -> bool
val toStringC : complex -> string
val isNegativeC : complex -> bool
val absComplex : complex -> complex