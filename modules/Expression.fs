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

// negates an expression
let neg e:Expr<Number> =
    match e with
    | N a -> N (-a)
    | _ -> Neg(e) 

// multiplies two expressions with simplification 
let rec mul e1 e2:Expr<Number> =
    match e1, e2 with
    |N a, N b                       -> N (a * b)
    |N a, b | b, N a when isOne a   -> b
    |N a, _ | _, N a when isZero a  -> N zero
    |Div(a, b), c | c, Div(a, b) -> Div (mul a c, b)
    | _, _                          -> Mul(e1, e2)

// adds two expressions
let rec add e1 e2:Expr<Number>  =
    match e1, e2 with
    | N a, N b                            -> N (a + b)
    | N a, b | b, N a when isZero a       -> b
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
    | Mul(a, X b), Mul(c, X d) 
    | Mul(X b, a), Mul(c, X d)
    | Mul(a, X b), Mul(X d, c) 
    | Mul(X b, a), Mul(X d, c) when b = d -> Mul(sub a c, X b) 
    | _, _ -> Sub(a, b)
 

// divides two expressions with simplification
let div e1 e2:Expr<Number> =
    match e1, e2 with
    | _, _   when e1 = e2     -> N one
    | _, N a when isOne a     -> e1
    | N a, _ when isZero a    -> N zero
    | _, N a when isZero a    ->  raise (System.DivideByZeroException("Expression.div: Cannot divide by zero!"))
    | N a, N b                -> N (a / b)
    | _, _                    -> Div(e1, e2)  



type Expr<'a> with
    static member (~-) (e)              = neg e
    static member (+)  (e1, e2)         = add e1 e2
    static member (-) (e1, e2)          = sub e1 e2
    static member (*) (e1, e2)          = mul e1 e2
    static member (/) (e1, e2)          = div e1 e2
    

// evaluates an expression without simplification, hence only using the Number operations
let rec eval (e:Expr<Number>) (env) =
    match e with
    | X x -> tryReduce(Map.find x env)
    | N n -> tryReduce(n)
    | Neg a -> - eval a env
    | Add (a, b) -> eval a env + eval b env
    | Sub (a, b) -> eval a env - eval b env
    | Mul (a, b) -> eval a env * eval b env
    | Div (a, b) -> eval a env / eval b env