module Vector
open Number


[<Sealed>]
type Vector =
    static member ( + )  : Vector * Vector -> Vector
    static member ( + )  : Vector * Number -> Vector
    static member ( + )  : Number * Vector -> Vector
