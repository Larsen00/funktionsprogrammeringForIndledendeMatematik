#load "Differentiation.fsx"
#load "AssociationForMulDiv.fs"
#load "AssociationForAddSub.fs"
open Expression
open Number
open rational

////////////////////////////////////////
/// SIMPLIFICATION FUNCTIONS ///////////
////////////////////////////////////////

// simplifies a negation expression
let neg e:Expr<Number> =
    match e with
    | Neg a -> a
    | _ -> Neg e

// simplifies a subtraction expression
let rec sub e1 e2:Expr<Number>=
    // printfn "s sub %A - %A" e1 e2
    match e1, e2 with
    | _, _ when e1 = e2 -> N zero
    | N a, _ when Number.isZero a -> neg e2
    | _, N a when Number.isZero a -> e1
    | N a, N b -> N (a - b)
    | Neg a, Neg b -> neg (sub a b)
    | a, Neg b -> a + b
    | Mul(a, X b), Mul(c, X d) when b = d -> Mul(sub a c, X b)
    | _, _ -> AssociationForAddSub.applyAssociation (Sub(e1, e2))

// simplifies a addition expression
let rec add e1 e2:Expr<Number> = 
    match e1, e2 with
    | N a, _ | _, N a when Number.isZero a -> e2
    | N a, N b -> N (a + b)
    | a, b when a = b -> Mul (N two, b)
    | Neg a, Neg b -> neg (add a b) 
    | a, Neg b -> sub a b
    | Mul(a, X b), Mul(c, X d) when b = d -> Mul(add a c, X b)
    | _, _ -> AssociationForAddSub.applyAssociation (Add(e1, e2))

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
    | Div (a, b), c | c , Div(a, b) -> div (AssociationForMulDiv.applyAssociation (Mul(a, c))) b
    | _, _ -> AssociationForMulDiv.applyAssociation (Mul(e1, e2))


// simplifies a division expression
and div e1 e2:Expr<Number> =
    match e1, e2 with
    | _, _ when e1 = e2 -> N one
    | _, N a when Number.isOne a -> e1
    |N a, _ when Number.isZero a -> N zero
    | _, N a when Number.isZero a -> failwith "Zero division"
    |N a, N b -> N(a / b)                    
    | Mul _, _ | _, Mul _ -> AssociationForMulDiv.applyAssociation (Div(e1, e2))
    | _,_ -> AssociationForMulDiv.applyAssociation (Div(e1, e2))



// Simplifies an Expression 
let rec simplifyExpr e =
    // printfn "%A" e
    match e with
    | N (Rational(R(a, b))) -> Div(N (Int a), N (Int b))
    | Neg a     -> neg (simplifyExpr a)
    | Add(a, b) -> add (simplifyExpr a) (simplifyExpr b) 
    | Sub(a, b) -> sub (simplifyExpr a) (simplifyExpr b) 
    | Mul(a, b) -> mul (simplifyExpr a) (simplifyExpr b) 
    | Div(a, b) -> div (simplifyExpr a) (simplifyExpr b)
    | _ -> e 

// replaces a variable with a expression
let replaceX c map =
    match Map.tryFind c map with
    | None -> X c
    | Some a -> a

// replaces all variables in a expression with a expression
let rec insert e map =
    match e with 
    | X a -> replaceX a map        
    | N _ -> e
    | Neg a -> Neg(insert a map)
    | Add(a, b) -> Add(insert a map, insert b map)
    | Sub(a, b) -> Sub(insert a map, insert b map)
    | Mul(a, b) -> Mul(insert a map, insert b map)
    | Div(a, b) -> Div(insert a map, insert b map)

// evaluates a expression using a map // #TODO tror ikke den virker
let eval e map = 
    let simplified = simplifyExpr (insert e map)
    insert simplified map
