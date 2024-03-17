module SymbolicManipolation

open Expression
open Number
open rational
open complex
open assoativeAddSub





/////////////////////////////////////////////////////////////////
/// ASSOCIATION RULES For multiplication and division ///////////
/////////////////////////////////////////////////////////////////

// rank when sorting a assoative expression
let expressionSortRankMD e1 =
    match e1 with
    | Neg _ -> 1
    | N _ -> 2
    | X _ -> 3
    | Add _ -> 4
    | Sub _ -> 5
    | Mul _ -> 6
    | Div _ -> 7
 
// sorts the assoative list
let rec sortAssMD l = List.sortBy (fun e -> expressionSortRankMD e) l


// determines the sign of a assoative list
let rec signList l s =
    match l with
    | [] -> s
    | Neg _::tail -> signList tail (-1*s)
    | _::tail -> signList tail s


// flattens the tree to a assoative list
let rec flatTreeMD e =
    match e with 
    | N _ | X _ | Add _ | Sub _ -> [e]
    | Neg a -> Neg (N one) :: flatTreeMD a
    | Mul (a, b) -> flatTreeMD a @ flatTreeMD b
    | Div (a, b) -> Div (N one, b) :: flatTreeMD a 


// multiplys all Div elements in a assoative list
let rec multiplyDivaInAssList l =
    // division will always be in the end of a sorted list, and numarator will be 1
    match l with
    | [] -> []
    | Div (_, b) :: Div (_, c) :: tail -> multiplyDivaInAssList (Div(N one, Mul(b, c)) :: tail)
    | x::tail -> x :: multiplyDivaInAssList tail

// removes a element from a list
let rec removeElem e l =
    match  l with
    | [] -> []
    | x::tail when x = e -> tail
    | x::tail -> x :: removeElem e tail

// reduces a assoative list
let rec reduceAss l =
    let sorted = divCancelling (sortAssMD l)
    // printfn "sorted: %A" sorted
    if signList sorted 1 > 0 then rebuildTree sorted else Neg (rebuildTree sorted)

// initiates the division cancelling
and divCancelling l = 
    // printfn "DivCancelling: %A" l
    match List.rev l with
    | [] -> l
    | Div(_, b)::tail -> 
                        let (numerator, denominator) = cancelEquality  (List.rev tail) (sortAssMD (flatTreeMD b))
                        sortAssMD (flatTreeMD (reduceAss numerator / reduceAss denominator))
    | _ -> l

// cancels out equal elements in the numerator and denominator
and cancelEquality nu de =
    // printfn "cancelEquality: %A / %A" nu de
    match nu with
    | [] -> ([], de)
    | n::ntail when List.contains n de ->  cancelEquality ntail (removeElem n de)
    | n::ntail -> let (numerator, denominator) = cancelEquality ntail de
                  (n::numerator, denominator)
                


// rebuilds a assoative list to a tree
and rebuildTree l =
    match l with
    | [] -> N one
    | Neg (N a)::tail when Number.isOne a -> rebuildTree tail
    | Neg a::tail -> rebuildTree (a::tail)
    | N a::N b::tail -> rebuildTree (N (a * b) :: tail)
    | N a :: Add(b, c) :: tail -> rebuildTree (Add(reduceAss (flatTreeMD (Mul(N a, b))), reduceAss (flatTreeMD (Mul(N a, c)))) :: tail)
    | N a :: Sub(b, c) :: tail -> rebuildTree (Sub(reduceAss (flatTreeMD (Mul(N a, b))), reduceAss (flatTreeMD (Mul(N a, c)))) :: tail)
    | a::tail -> a * rebuildTree tail


// applies the assoation rules to a tree    
let applyAssociation e:Expr<Number> =
    match e with
    | Mul _ | Div _ -> reduceAss (flatTreeMD e)
    | _ -> e



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
    | _, _ -> assoativeAddSub.applyAssociation (Add(e1, e2))

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
    | _, _ -> assoativeAddSub.applyAssociation (Sub(e1, e2))





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
    // | Div (a, b), c | c , Div(a, b) -> div (applyAssociationMD (Mul(a, c))) b
    | _, _ -> applyAssociation (Mul(e1, e2))



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
    | _,_ -> applyAssociation (Div(e1, e2))



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

