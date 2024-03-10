module AssociationForMulDiv
open Association
open Number
open Expression

/////////////////////////////////////////////////////////////////
/// ASSOCIATION RULES For multiplication and division ///////////
/////////////////////////////////////////////////////////////////

// rank when sorting a assoative expression
let expressionSortRank e1 =
    match e1 with
    | Neg _ -> 1
    | N _ -> 2
    | X _ -> 3
    | Add _ -> 4
    | Sub _ -> 5
    | Mul _ -> 6
    | Div _ -> 7
 
// sorts the assoative list
let rec sortAss l = List.sortBy (fun e -> expressionSortRank e) l


// determines the sign of a assoative list
let rec signList l s =
    match l with
    | [] -> s
    | Neg _::tail -> signList tail (-1*s)
    | _::tail -> signList tail s


// flattens the tree to a assoative list
let rec flatTree e =
    match e with 
    | N _ | X _ | Add _ | Sub _ -> [e]
    | Neg a -> Neg (N one) :: flatTree a
    | Mul (a, b) -> flatTree a @ flatTree b
    | Div (a, b) -> Div (N one, b) :: flatTree a 


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
    let sorted = divCancelling (sortAss l)
    // printfn "sorted: %A" sorted
    if signList sorted 1 > 0 then rebuildTree sorted else Neg (rebuildTree sorted)

// initiates the division cancelling
and divCancelling l = 
    // printfn "DivCancelling: %A" l
    match List.rev l with
    | [] -> l
    | Div(_, b)::tail -> 
                        let (numerator, denominator) = cancelEquality  (List.rev tail) (sortAss (flatTree b))
                        // printfn "numerator: %A, denominator: %A" numerator denominator
                        // sortAss (flatTree (rebuildTree numerator / rebuildTree denominator))
                        sortAss (flatTree (reduceAss numerator / reduceAss denominator))
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
    | Neg (N a)::tail when isOne a -> rebuildTree tail
    | Neg a::tail -> rebuildTree (a::tail)
    | N a::N b::tail -> rebuildTree (N (a * b) :: tail)
    | N a :: Add(b, c) :: tail -> rebuildTree (Add(reduceAss (flatTree (Mul(N a, b))), reduceAss (flatTree (Mul(N a, c)))) :: tail)
    | N a :: Sub(b, c) :: tail -> rebuildTree (Sub(reduceAss (flatTree (Mul(N a, b))), reduceAss (flatTree (Mul(N a, c)))) :: tail)
    | a::tail -> a * rebuildTree tail


// applies the assoation rules to a tree    
let applyAssociation e:Expr<Number> =
    match e with
    | Mul _ | Div _ -> reduceAss (flatTree e)
    | _ -> e

