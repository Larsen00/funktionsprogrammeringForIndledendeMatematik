module Expression
open Number

[<Sealed>]
type Expr<'a> =
    static member ( + )  : Expr<Number> * Expr<Number> -> Expr<Number>
    static member ( ~- ) : Expr<Number> -> Expr<Number>
    static member ( - )  : Expr<Number> * Expr<Number> -> Expr<Number>
    static member ( * )  : Expr<Number> * Expr<Number> -> Expr<Number>
    static member ( / )  : Expr<Number> * Expr<Number> -> Expr<Number>

