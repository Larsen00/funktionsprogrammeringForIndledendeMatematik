module Expression
open Number

type Expr<'a> = 
            | X of char
            | N of 'a
            | Add of Expr<'a> * Expr<'a>
            | Neg of Expr<'a>
            | Sub of Expr<'a> * Expr<'a>
            | Mul of Expr<'a> * Expr<'a>
            | Div of Expr<'a> * Expr<'a>

let neg e:Expr<Number> =
    match e with
    | Neg a -> a
    | _ -> Neg e

let rec add e1 e2 = 
    match e1, e2 with
    | N a, _ when isZero a -> e2
    | _, N a when isZero a -> e1
    | N a, N b -> N (a + b)
    | a, b when a = b -> Mul (N two, b)
    | Neg a, Neg b -> neg (add a b) 
    | a, Neg b -> sub a b 
    | _, _ -> Add (e1, e2)
    
and  sub e1 e2:Expr<Number> =
    match e1, e2 with
    | _, _ when e1 = e2 -> N zero
    |N a, _ when isZero a -> neg e2
    | _, N a when isZero a -> e1
    |N a, N b -> N (a - b)
    | Neg a, Neg b -> neg (sub a b) 
    | a, Neg b -> neg (add a b) 
    | _, _ -> Sub(e1, e2)
    

let rec mul e1 e2 =
    match e1 ,e2 with
    |N a, N b -> N (a * b)
    |N a, _ when isOne a -> e2
    | _, N a when isOne a -> e1
    |N a, _ when isZero a -> N zero
    | _, N a when isZero a -> N zero
    |Neg a, Neg b -> mul a b
    |Neg a, b -> neg (mul a b)
    |a, Neg b -> neg (mul a b)
    |Mul _, Mul _ -> 
                    let (m1, k1) = assosiativeRule2 e1
                    let (m2, k2) = assosiativeRule2 e2
                    mul (N (k1*k2)) (Mul(m1, m2))
    |Mul _, b -> assosiativeRule1 e1 b
    |a , Mul _ -> assosiativeRule1 e2 a
    |Div (a, b), Div (c, d) -> div (mul a c) (mul b d) 
    | _,_ -> Mul (e1, e2)
and assosiativeRule1 e1 e2 =
    match e1, e2 with
    | Mul(N a, b), N c  -> Mul (N (a*c), b)
    | Mul(a, N b), N c -> Mul (N (b*c), a)
    | Mul(N a, b), _  -> Mul (N a, Mul(b, e2))
    | Mul(a, N b), _ -> Mul (N b, Mul(a, e2))
    | _ -> Mul (e1, e2)
and assosiativeRule2 e =
    match e with
    | Mul (N a, N b) -> (N one, a * b)
    | Mul (N a, b) -> (b, a)
    | Mul (a, N b) -> (a, b)
    | _ -> (e, one)


// #ToDo: mangler cases
and div e1 e2 =
    match e1, e2 with
    | _, _ when e1 = e2 -> N one
    | _, N a when isOne a -> e1
    |N a, _ when isZero a -> N zero
    | _, N a when isZero a ->  raise (System.DivideByZeroException("Expression.div: Cannot divide by zero!"))
    |N a, N b -> N(a / b)                    
    |Mul(a, b), c -> mul (div a c) (div b c)
    | _,_ -> Div (e1, e2)


type Expr<'a> with
    static member (+)  (e1, e2)         = add e1 e2
    static member (~-) (e)              = neg e
    static member (-) (e1, e2)          = sub e1 e2
    static member (*) (e1, e2)          = mul e1 e2
    static member (/) (e1, e2)          = div e1 e2
    