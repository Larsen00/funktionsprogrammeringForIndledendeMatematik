#load "Differentiation.fsx"
open Expression
open Number


let expressionSortRank e1 =
    match e1 with
    | Neg _ -> 1
    | N _ -> 2
    | X _ -> 3
    | Add _ -> 4
    | Sub _ -> 5
    | Mul _ -> 6
    | Div _ -> 7
 
// flattens the tree to a list, where each element is multiplicated
let rec flatTree e =
    match e with 
    | N _ | X _ | Add _ | Sub _ -> [e]
    | Neg a -> Neg (N one) :: flatTree a
    | Mul (a, b) -> flatTree a @ flatTree b
    | Div (a, b) -> Div (N one, b) :: flatTree a 

let rec sortAss l = List.sortBy (fun e -> expressionSortRank e) l



let rec signList l s =
    match l with
    | [] -> s
    | Neg _::tail -> signList tail (-1*s)
    | _::tail -> signList tail s



let rec concatDivSortedList l =
    // division will always be in the end of a sorted list, and numarator will be 1
    match l with
    | [] -> []
    | Div (_, b) :: Div (_, c) :: tail -> concatDivSortedList (Div(N one, Mul(b, c)) :: tail)
    | x::tail -> x :: concatDivSortedList tail


let rec reduceAss l =
    let sorted = divCancelling (sortAss l)
    // printfn "%A" sorted
    if signList sorted 1 > 0 then rebuildTree sorted else Neg (rebuildTree sorted)

and divCancelling l = 
    printfn "%A" l
    match List.rev l with
    | [] -> l
    | Div(_, b)::tail -> 
                        let (numerator, denominator) = cancelEquality  l (sortAss (flatTree b))
                        sortAss (flatTree (rebuildTree numerator / rebuildTree denominator))
    | _ -> l
and cancelEquality nu de =
    match nu with
    | [] -> ([], [])
    | n::ntail -> 
                match checkElementInNumerator n de with
                | false, _ ->  
                            let (numerator, denominator) = cancelEquality ntail de
                            (n::numerator, denominator)
                | true, de_new -> cancelEquality ntail de_new

and checkElementInNumerator e de =
    match de with
    | [] -> false , []
    | d::tail when d = e -> true, tail
    | _::tail -> checkElementInNumerator e tail

and rebuildTree l =
    match l with
    | [] -> N one
    | Neg _::tail -> rebuildTree tail
    | N a::N b::tail -> rebuildTree (N (a * b) :: tail)
    | N a :: Add(b, c) :: tail -> rebuildTree (Add(reduceAss (flatTree (Mul(N a, b))), reduceAss (flatTree (Mul(N a, c)))) :: tail)
    | N a :: Sub(b, c) :: tail -> rebuildTree (Sub(reduceAss (flatTree (Mul(N a, b))), reduceAss (flatTree (Mul(N a, c)))) :: tail)
    | a::tail -> a * rebuildTree tail



    
let applyAssociation e =
    match e with
    | Mul _ | Div _ -> reduceAss (flatTree e)
    | _ -> e




////////////////////////////////////////
/// SIMPLIFICATION FUNCTIONS ///////////
////////////////////////////////////////
let neg e =
    match e with
    | Neg a -> a
    | _ -> Neg e

let rec sub e1 e2 =
    match e1, e2 with
    | _, _ when e1 = e2 -> N zero
    | N a, _ when isZero a -> neg e2
    | _, N a when isZero a -> e1
    | N a, N b -> N (a - b)
    | Neg a, Neg b -> neg (sub a b) 
    | a, Neg b -> neg (a + b) 
    | _, _ -> Sub(e1, e2)

let rec add e1 e2 = 
    match e1, e2 with
    | N a, _ | _, N a when isZero a -> e2
    | N a, N b -> N (a + b)
    | a, b when a = b -> Mul (N two, b)
    | Neg a, Neg b -> neg (add a b) 
    | a, Neg b -> sub a b 
    | _, _ -> Add (e1, e2)

open TreeGenerator
let rec mul e1 e2 =
    //printfn "\n 81 - %A" (ExpressionToInfix (Mul (e1, e2)) false)
    match e1 ,e2 with
    | N _, N _ -> e1 * e2
    | N a, _ when isOne a -> e2
    | _, N a when isOne a -> e1
    | N a, _ when isZero a  -> N zero
    | _, N a when isZero a  -> N zero
    | Neg a, Neg b                   -> mul a b
    | Neg a, b | a, Neg b            -> neg (mul a b)
    | Div (a, b), Div (c, d) -> div (mul a c) (mul b d)
    | Div (a, b), c | c , Div(a, b) -> div (applyAssociation (Mul(a, c))) b
    | _, _ -> applyAssociation (Mul(e1, e2))




// #ToDo: mangler cases
and div e1 e2 =
    match e1, e2 with
    | _, _ when e1 = e2 -> N one
    | _, N a when isOne a -> e1
    |N a, _ when isZero a -> N zero
    | _, N a when isZero a -> failwith "Zero division"
    |N a, N b -> N(a / b)                    
    //|Mul(a, b), c -> mul (div a c) (div b c)
    | Mul _, _ | _, Mul _ -> applyAssociation (Div(e1, e2))
    | _,_ -> applyAssociation (Div(e1, e2))



// Simplifies an Expression 
let rec simplifyExpr e =
    match e with
    | Neg a     -> neg (simplifyExpr a)
    | Add(a, b) -> add (simplifyExpr a) (simplifyExpr b) 
    | Sub(a, b) -> sub (simplifyExpr a) (simplifyExpr b) 
    | Mul(a, b) -> mul (simplifyExpr a) (simplifyExpr b) 
    | Div(a, b) -> div (simplifyExpr a) (simplifyExpr b)
    | _ -> e 


let replaceX c map =
    match Map.tryFind c map with
    | None -> X c
    | Some a -> a

let rec insert e map =
    match e with 
    | X a -> replaceX a map        
    | N _ -> e
    | Neg a -> Neg(insert a map)
    | Add(a, b) -> Add(insert a map, insert b map)
    | Sub(a, b) -> Sub(insert a map, insert b map)
    | Mul(a, b) -> Mul(insert a map, insert b map)
    | Div(a, b) -> Div(insert a map, insert b map)