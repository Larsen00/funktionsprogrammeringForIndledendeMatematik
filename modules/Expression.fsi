module Expression
open Number

type Expr<'a> = 
  | X of char
  | N of 'a
  | Neg of Expr<'a>
  | Add of Expr<'a> * Expr<'a>
  | Sub of Expr<'a> * Expr<'a>
  | Mul of Expr<'a> * Expr<'a>
  | Div of Expr<'a> * Expr<'a>
with
  static member ( ~- ) : Expr<Number> -> Expr<Number>
  static member ( + ) : Expr<Number> * Expr<Number> -> Expr<Number>
  static member ( - ) : Expr<Number> * Expr<Number> -> Expr<Number>
  static member ( * ) : Expr<Number> * Expr<Number> -> Expr<Number>
  static member ( / ) : Expr<Number> * Expr<Number> -> Expr<Number>

val eval : Expr<Number> -> Map<char,Number> -> Number
val isAdd : (Expr<Number> -> Expr<Number> -> Expr<Number>) -> bool
val isSub : (Expr<Number> -> Expr<Number> -> Expr<Number>) -> bool
val isMul : (Expr<Number> -> Expr<Number> -> Expr<Number>) -> bool
val isDiv : (Expr<Number> -> Expr<Number> -> Expr<Number>) -> bool
val isNeg : (Expr<Number> -> Expr<Number>) -> bool
val containsX : Expr<Number> -> Expr<Number> -> bool
val getNumber : Expr<Number> -> Number
val getVariable : Expr<Number> -> char
