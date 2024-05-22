module SymbolicManipolation

open Expression
open Number
open rational


////////////////////////////////////////
/// SIMPLIFICATION FUNCTIONS ///////////
////////////////////////////////////////

let rec simplifyOperation e1 e2 f = 
    match f e1 e2 with
    | Neg a -> CommutativeMulDiv.applyCommutative (Neg a) |> CommutativeAddSub.applyCommutative
    | Add(a, b) when isAdd f -> CommutativeAddSub.applyCommutative (Add(a, b))
    | Sub(a, b) when isSub f -> CommutativeAddSub.applyCommutative (Sub(a, b))
    | Mul(a, b) when isMul f -> CommutativeMulDiv.applyCommutative (Mul(a, b))
    | Div(a, b) when isDiv f -> CommutativeMulDiv.applyCommutative (Div(a, b))
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
let rec expressionOnX hs x =
    match hs with
    | N _ | X _ -> fun e -> e
    | Neg(a) when a = x -> fun e -> Neg e
    | Sub(a, b) when a = x -> fun e -> Add(e, b)
    | Div(a, b) when a = x -> fun e -> Mul(e, b)
    | Div(_, a) when a = x -> fun e -> Mul(e, a)
    | Mul(a, b) | Mul(b, a) when a = x -> fun e -> Div(e, b)
    | Add(a, b) | Add(b, a) | Sub(b, a) when a = x -> fun e -> Sub(e, b)
    | Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) -> fun e -> expressionOnX a x e |> expressionOnX b x 
    | Neg(a) -> fun e -> expressionOnX a x e

// Isolates X in an equation
// Cant handle to complex equations, well suited for multivariable polynomials of degree 1
let rec isolateX lhs rhs x =
    let operation = 
        if containsX lhs x 
            then expressionOnX lhs x
        elif containsX rhs x 
            then expressionOnX rhs x
        else 
            failwith "Variable not found in either side of the equation"
    match operation lhs |> simplifyExpr, operation rhs |> simplifyExpr with
    | a, b | b, a when a = x -> (a, b)
    | a, b -> isolateX a b x


