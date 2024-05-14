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

// negates an expression (Wants to avoid negative N)
let rec neg e:Expr<Number> =
    match e with
    | Neg a -> whileNeg a
    | _ -> Neg e 
and whileNeg e =
    match e with
    | Neg a -> neg a
    | _ -> e



// multiplies two expressions with simplification 
let rec mul e1 e2:Expr<Number> =
    match e1, e2 with
    |N a, N b                       -> N (a * b)
    |N a, b | b, N a when isOne a   -> b
    |N a, _ | _, N a when isZero a  -> N zero
    |Div(a, b), c | c, Div(a, b)    -> Div (mul a c, b)
    |Div (a, b), Div (c, d)         -> Div ((mul a c), (mul b d))
    |Neg a, Neg b                   -> mul a b
    | _, _                          -> Mul(e1, e2)

// adds two expressions
let rec add e1 e2:Expr<Number>  =
    match e1, e2 with
    | N a, N b                            -> N (a + b)
    | N a, b | b, N a when isZero a       -> b
    | a, b when a = b                     -> Mul (N two, b)
    | Neg a, Neg b                        -> neg (add a b) 
    | Neg a, b | b, Neg a                 -> Sub (b, a)
    | Mul(a, X b), Mul(c, X d) 
    | Mul(X b, a), Mul(c, X d)
    | Mul(a, X b), Mul(X d, c) 
    | Mul(X b, a), Mul(X d, c) when b = d -> Mul(add a c, X b)
    | _, _                                -> Add(e1, e2)

// subtracts two expressions
let rec sub a b:Expr<Number>  =
    match a, b with
    | _, _ when a = b -> N zero
    | N x, N y  when greaterThan y x -> Neg (N (y - x)) // Want to avoid negative numbers
    | N x, N y -> N (x - y)
    | N a, b when isZero a -> Neg b
    | a, N b when isZero b -> a
    | a, Neg b -> add a b 
    | Neg a, Neg b -> neg (sub a b)
    | Mul(a, X b), Mul(c, X d) 
    | Mul(X b, a), Mul(c, X d)
    | Mul(a, X b), Mul(X d, c) 
    | Mul(X b, a), Mul(X d, c) when b = d -> Mul(sub a c, X b) 
    | _, _ -> Sub(a, b)
 

// divides two expressions with simplification
let rec div e1 e2:Expr<Number> =
    match e1, e2 with
    | _, N a when isZero a    ->  raise (System.DivideByZeroException("Expression.div: Cannot divide by zero!"))
    | _, _   when e1 = e2     -> N one
    | _, N a when isOne a     -> e1
    | N a, _ when isZero a    -> N zero
    | N a, N b                -> N (a / b)
    | Neg a, Neg b            -> div a b
    | a, Neg b | Neg a, b     -> neg (div a b) 
    | Div(a, b), c            -> div a (mul b c) 
    | _, _                    -> Div(e1, e2)  



type Expr<'a> with
    static member (~-) (e)              = neg e
    static member (+)  (e1, e2)         = add e1 e2
    static member (-) (e1, e2)          = sub e1 e2
    static member (*) (e1, e2)          = mul e1 e2
    static member (/) (e1, e2)          = div e1 e2

let rec containsX t (v:Expr<Number>) =
    match t with
    | _ when t = v -> true
    | Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) -> containsX a v || containsX b v
    | Neg a -> containsX a v
    | _ -> false


// evaluates an expression without simplification, hence only using the Number operations
let rec eval (e:Expr<Number>) (env) =
    match e with
    | X x -> Map.find x env
    | N n -> n
    | Neg a -> - eval a env
    | Add (a, b) -> eval a env + eval b env
    | Sub (a, b) -> eval a env - eval b env
    | Mul (a, b) -> eval a env * eval b env
    | Div (a, b) -> eval a env / eval b env

let getNumber (e:Expr<Number>) =
    eval e Map.empty

let getVariable (e:Expr<Number>) =
    match e with
    | X x -> x
    | _ -> raise (System.Exception("Expression.getChar: Expression is not a variable!"))

let isZero (e:Expr<Number>) =
    match e with
    | N n ->  Number.isZero n
    | _ -> false

// functions for equality check on operations
let isAdd (f:(Expr<Number> -> Expr<Number> -> Expr<Number>)) = 
    f (X 'x') (X 'y') = Add(X 'x', X 'y')

let isSub (f:(Expr<Number> -> Expr<Number> -> Expr<Number>)) =
    f (X 'x') (X 'y') = Sub(X 'x', X 'y')

let isMul (f:(Expr<Number> -> Expr<Number> -> Expr<Number>)) =
    f (X 'x') (X 'y') = Mul(X 'x', X 'y')

let isDiv (f:(Expr<Number> -> Expr<Number> -> Expr<Number>)) = 
    f (X 'x') (X 'y') = Div(X 'x', X 'y')

let isNeg (f:(Expr<Number> -> Expr<Number>)) = 
    f (X 'x') = Neg(X 'x')