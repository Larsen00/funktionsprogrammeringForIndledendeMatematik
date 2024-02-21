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
    | N a -> N (-a)
    | _ -> Neg(e) 

let add e1 e2:Expr<Number> =
    match e1, e2 with
    | N a, N b -> N (a + b)
    | _ -> Add(e1, e2)

let sub e1 e2:Expr<Number> =
    match e1, e2 with
    | N a, N b -> N (a - b)
    | _ -> Sub(e1, e2)    

let mul e1 e2:Expr<Number> =
    match e1, e2 with
    |N a, N b                       -> N (a * b)
    |N a, b | b, N a when isOne a   -> b
    |N a, _ | _, N a when isZero a  -> N zero
    | _, _ -> Mul(e1, e2)

let div e1 e2:Expr<Number> =
    match e1, e2 with
    | _, _ when e1 = e2 -> N one
    | _, N a when isOne a -> e1
    |N a, _ when isZero a -> N zero
    | _, N a when isZero a -> failwith "Zero division"
    |N a, N b -> N(a / b)
    | _, _ -> Div(e1, e2)  

type Expr<'a> with
    static member (~-) (e)              = neg e
    static member (+)  (e1, e2)         = add e1 e2
    static member (-) (e1, e2)          = sub e1 e2
    static member (*) (e1, e2)          = mul e1 e2
    static member (/) (e1, e2)          = div e1 e2
    

