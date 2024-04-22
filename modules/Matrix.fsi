module Matrix
open Number

type Order = | R | C
type Dimension = D of int * int

type Vector =
    V of Number list * Order
    with
        static member ( + )  : Vector * Vector -> Vector
        static member ( + )  : Vector * Vector -> Vector
        static member ( * )  : Number * Vector -> Vector
        static member ( * )  : Vector * Number -> Vector 
        static member ( ~- ) : Vector -> Vector

type Matrix =
    M of Vector list * Order
    with
        static member ( + )  : Matrix * Matrix -> Matrix
        static member ( + )  : Matrix * Number -> Matrix
        static member ( + )  : Number * Matrix -> Matrix
        static member ( * )  : Number * Matrix -> Matrix
        static member ( * )  : Matrix * Number -> Matrix
        static member ( * )  : Matrix * Vector -> Matrix
        static member ( * )  : Matrix * Matrix -> Matrix
        static member ( / )  : Matrix * Number -> Matrix

val vector : Number list -> Vector
val matrix : Vector list -> Matrix
val matrixOf : Number -> Dimension -> Matrix
val sumRows : Matrix -> Matrix
val flip : Matrix -> Matrix
val orthogonalBacis : Matrix -> Matrix
val rowEchelonForm : Matrix -> Matrix
val stringMatrix : Matrix -> string
val standardBacis : int -> Matrix
val rowOperation : int -> int -> Number -> Matrix -> Matrix
val dimMatrix : Matrix -> Dimension
val isOrthogonalBacis : Matrix -> bool
val Axequalb : Matrix -> Vector -> Vector
val extendMatrix : Matrix -> list<Number> -> Matrix
val fullrankedDiagonalMatrix : int -> int -> Matrix
val hasFullRank : Matrix -> bool
val transposeMatrix : Matrix -> Matrix
val hasSameSpan : Matrix -> Matrix -> bool