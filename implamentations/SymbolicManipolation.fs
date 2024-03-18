module SymbolicManipolation

open Expression
open Number
open rational
open complex
open commutativeAddSub
open commutativeMulDiv





////////////////////////////////////////
/// SIMPLIFICATION FUNCTIONS ///////////
////////////////////////////////////////

// simplifies a negation expression
let neg e:Expr<Number> =
    match e with
    | Neg a -> a
    | _ -> Neg e

// simplifies a addition expression
let rec add e1 e2 =
    match e1 + e2 with
    | Neg a -> neg a
    | Add(a, b) -> add_simp a b
    | a -> a

and add_simp e1 e2:Expr<Number> = 
    match e1, e2 with
    | a, b when a = b -> Mul (N two, b)
    | Neg a, Neg b -> neg (add a b) 
    | a, Neg b -> a - b
    | _, _ -> commutativeAddSub.applyCommutative (Add(e1, e2))

// simplifies a subtraction expression
let rec sub e1 e2 =
    match e1 - e2 with
    | Neg a -> neg a
    | Add(a, b) -> add a b
    | Sub(a, b) -> sub_simp a b
    | a -> a
and sub_simp e1 e2:Expr<Number>=
    match e1, e2 with
    | Neg a, Neg b -> neg (sub a b)
    | _, _ -> commutativeAddSub.applyCommutative (Sub(e1, e2))





// simplifies a multiplication expression
let rec mul e1 e2:Expr<Number> =
    //printfn "\n 81 - %A" (ExpressionToInfix (Mul (e1, e2)) false)
    match e1 ,e2 with
    | N _, N _ -> e1 * e2
    | N a, _ when Number.isOne a -> e2
    | _, N a when Number.isOne a -> e1
    | N a, _ when Number.isZero a  -> N zero
    | _, N a when Number.isZero a  -> N zero
    | Neg a, Neg b                   -> mul a b
    | Neg a, b | a, Neg b            -> neg (mul a b)
    | Div (a, b), Div (c, d) -> div (mul a c) (mul b d)
    | _, _ -> commutativeMulDiv.applyCommutative (Mul(e1, e2))



// simplifies a division expression
and div e1 e2:Expr<Number> =
    // printfn "%A / %A" e1 e2
    match e1, e2 with
    | _, _ when e1 = e2 -> N one
    | _, N a when Number.isOne a -> e1
    |N a, _ when Number.isZero a -> N zero
    | _, N a when Number.isZero a ->  raise (System.DivideByZeroException("SymbolicManipolation.div: Cannot divide by zero!"))
    |N a, N b -> N(a / b)
    | Neg a, Neg b -> div a b
    | a, Neg b -> neg (div a b)                    
    | _,_ -> commutativeMulDiv.applyCommutative (Div(e1, e2))



// Simplifies an Expression 
let rec simplifyExpr e =
    // printfn "%A" e
    match e with
    | N a when Number.isNegative a -> Neg (N (Number.absNumber a))
    | N (Rational(R(a, b))) -> div (N (Int a)) (N (Int b))
    | Neg a     -> neg (simplifyExpr a)
    | Add(a, b) -> add (simplifyExpr a) (simplifyExpr b) 
    | Sub(a, b) -> sub (simplifyExpr a) (simplifyExpr b) 
    | Mul(a, b) -> mul (simplifyExpr a) (simplifyExpr b) 
    | Div(a, b) -> div (simplifyExpr a) (simplifyExpr b)
    | _ -> e 

