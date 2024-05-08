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

let rec simplifyOperation e1 e2 f = 
    match f e1 e2 with
    | Neg a -> commutativeMulDiv.applyCommutative (Neg a) |> commutativeAddSub.applyCommutative
    | Add(a, b) when isAdd f -> commutativeAddSub.applyCommutative (Add(a, b))
    | Sub(a, b) when isSub f -> commutativeAddSub.applyCommutative (Sub(a, b))
    | Mul(a, b) when isMul f -> commutativeMulDiv.applyCommutative (Mul(a, b))
    | Div(a, b) when isDiv f -> commutativeMulDiv.applyCommutative (Div(a, b))
    | Add(a, b) -> simplifyOperation a b (+)
    | Sub(a, b) -> simplifyOperation a b (-)
    | Mul(a, b) -> simplifyOperation a b (*)
    | Div(a, b) -> simplifyOperation a b (/)
    | a -> a


// Simplifies an Expression 
let rec simplifyExpr e =
    match e with
    | N a when Number.isNegative a -> Neg (N (Number.absNumber a))
    | N (Rational(R(a, b))) -> simplifyOperation (simplifyExpr (N (Int a))) (simplifyExpr (N (Int b))) (/)
    | Neg a     -> - (simplifyExpr a)
    | Add(a, b) -> simplifyOperation (simplifyExpr a) (simplifyExpr b) (+)
    | Sub(a, b) -> simplifyOperation (simplifyExpr a) (simplifyExpr b) (-)
    | Mul(a, b) -> simplifyOperation (simplifyExpr a) (simplifyExpr b) (*)
    | Div(a, b) -> simplifyOperation (simplifyExpr a) (simplifyExpr b) (/)
    | _ -> e 




// // simplifies a addition expression
// let rec add e1 e2 =
//     match e1 + e2 with
//     | Neg a -> neg a
//     | Add(a, b) -> commutativeAddSub.applyCommutative (Add(a, b))
//     | Mul(a, b) -> mul a b
//     | Sub(a, b) -> sub a b
//     | Div(a, b) -> div a b
//     | a -> a

// // simplifies a subtraction expression
// and sub e1 e2 =
//     match e1 - e2 with
//     | Neg a -> neg a
//     | Add(a, b) -> add a b
//     | Mul(a, b) -> mul a b
//     | Sub(a, b) -> commutativeAddSub.applyCommutative (Sub(a, b))
//     | a -> a

// // simplifies a multiplication expression
// and mul e1 e2:Expr<Number> =
//     match e1 * e2 with
//     | Neg a -> neg a
//     | Add(a, b) -> add a b
//     | Sub(a, b) -> sub a b
//     | Div(a, b) -> div a b
//     | Mul(a, b) -> commutativeMulDiv.applyCommutative (Mul(a, b))
//     | a -> a

// // simplifies a division expression
// and div e1 e2:Expr<Number> =
//     match e1 / e2 with
//     | Neg a -> neg a
//     | Add(a, b) -> add a b
//     | Sub(a, b) -> sub a b
//     | Mul(a, b) -> mul a b            
//     | Div(a, b) -> commutativeMulDiv.applyCommutative (Div(a, b))
//     | a -> a





// // Simplifies an Expression 
// let rec simplifyExpr e =
//     match e with
//     | N a when Number.isNegative a -> Neg (N (Number.absNumber a))
//     | N (Rational(R(a, b))) -> div (N (Int a)) (N (Int b))
//     | Neg a     -> neg (simplifyExpr a)
//     | Add(a, b) -> add (simplifyExpr a) (simplifyExpr b) 
//     | Sub(a, b) -> sub (simplifyExpr a) (simplifyExpr b) 
//     | Mul(a, b) -> mul (simplifyExpr a) (simplifyExpr b) 
//     | Div(a, b) -> div (simplifyExpr a) (simplifyExpr b)
//     | _ -> e 

// insert envirement into an expression
let rec insertEnv e env =
    match e with
    | X a when Map.containsKey a env -> N (Map.find a env)
    | Neg a -> Neg (insertEnv a env)
    | Add(a, b) -> Add(insertEnv a env, insertEnv b env)
    | Sub(a, b) -> Sub(insertEnv a env, insertEnv b env)
    | Mul(a, b) -> Mul(insertEnv a env, insertEnv b env)
    | Div(a, b) -> Div(insertEnv a env, insertEnv b env)
    | _ -> e


// Finds X in an expression and returns the reverse expression
let rec expressionOnX t v =
    match t with
    | N _ | X _ -> fun a -> a
    | Neg(x) when x = v -> fun a -> Neg a
    | Add(x, y) | Add(y, x) | Sub(y, x) when x = v -> fun a -> Sub(a, y)
    | Sub(x, y) when x = v -> fun a -> Add(a, y)
    | Mul(x, y) | Mul(y, x) when x = v -> fun a -> Div(a, y)
    | Div(x, y) when x = v -> fun a -> Mul(a, y)
    | Div(_, x) when x = v -> fun a -> Mul(a, x)
    | Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) -> fun a -> expressionOnX x v a |> expressionOnX y v 
    | Neg(x) -> fun a -> expressionOnX x v a

// Isolates X in an equation
// Cant handle to complex equations, well suited for multivariable polynomials of degree 1
let rec isolateX lhs rhs v =
    let operation = 
        if containsX lhs v 
        then expressionOnX lhs v
        elif containsX rhs v 
        then expressionOnX rhs v
        else failwith "Variable not found in either side of the equation"
    match operation lhs |> simplifyExpr, operation rhs |> simplifyExpr with
    | x, y | y, x when x = v -> (x, y)
    | x, y -> isolateX x y v

