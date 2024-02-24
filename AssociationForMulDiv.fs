module AssociationForMulDiv
open Association
open Expression
open Number

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

// reduces a assoative list
let rec reduceAss l =
    let sorted = divCancelling (sortAss l)
    if signList sorted 1 > 0 then rebuildTree sorted else Neg (rebuildTree sorted)

// initiates the division cancelling
and divCancelling l = 
    match List.rev l with
    | [] -> l
    | Div(_, b)::tail -> 
                        let (numerator, denominator) = cancelEquality  (List.rev tail) (sortAss (flatTree b))
                        sortAss (flatTree (rebuildTree numerator / rebuildTree denominator))
    | _ -> l

// cancels out equal elements in the numerator and denominator
and cancelEquality nu de =
    match nu with
    | [] -> ([], [])
    | n::ntail -> 
                match checkElementInNumerator n de with
                | false, _ ->  
                            let (numerator, denominator) = cancelEquality ntail de
                            (n::numerator, denominator)
                | true, de_new -> cancelEquality ntail de_new

// determines if a element is in the numerator
and checkElementInNumerator e de =
    match de with
    | [] -> false , []
    | d::tail when d = e -> true, tail
    | _::tail -> checkElementInNumerator e tail

// rebuilds a assoative list to a tree
and rebuildTree l =
    match l with
    | [] -> N one
    | Neg _::tail -> rebuildTree tail
    | N a::N b::tail -> rebuildTree (N (a * b) :: tail)
    | N a :: Add(b, c) :: tail -> rebuildTree (Add(reduceAss (flatTree (Mul(N a, b))), reduceAss (flatTree (Mul(N a, c)))) :: tail)
    | N a :: Sub(b, c) :: tail -> rebuildTree (Sub(reduceAss (flatTree (Mul(N a, b))), reduceAss (flatTree (Mul(N a, c)))) :: tail)
    | a::tail -> a * rebuildTree tail


// applies the assoation rules to a tree    
let applyAssociation e:Expr<Number> =
    match e with
    | Mul _ | Div _ -> reduceAss (flatTree e)
    | _ -> e

