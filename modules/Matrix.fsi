module Matrix
open Number

type order = | R | C

type Vector =  V of Number list * order
type Dimension = D of int * int

type Matrix =
    M of Vector list * order
    with
        static member ( + )  : Matrix * Matrix -> Matrix
        static member ( + )  : Matrix * Number -> Matrix
        static member ( + )  : Number * Matrix -> Matrix
        static member ( * )  : Number * Matrix -> Matrix
        static member ( * )  : Matrix * Number -> Matrix

val vector : Number list -> Vector
val matrix : Vector list -> Matrix
val matrixOf : Number -> Dimension -> Matrix
val sumRows : Matrix -> Matrix
val flip : Matrix -> Matrix
