
module Rational
[<Sealed>]
type Rational =
    static member ( ~- ) : Rational -> Rational
    static member ( + )  : Rational * Rational -> Rational
    static member ( - )  : Rational * Rational -> Rational
    static member ( * )  : int  * Rational -> Rational
    static member ( * )  : Rational * int -> Rational
    static member ( * )  : Rational * Rational -> Rational
    static member ( / )  : Rational * Rational -> Rational
    static member ( / )  : int * Rational -> Rational
    static member ( / )  : Rational * int -> Rational
val make : int * int -> Rational
val equal : Rational * Rational -> bool
val posetive : Rational -> bool
val toString : Rational -> string
val isZero : Rational -> bool
val isOne : Rational -> bool