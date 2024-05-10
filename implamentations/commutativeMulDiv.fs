module commutativeMulDiv
open Expression
open Number


/////////////////////////////////////////////////////////////////
/// Commutative RULES For multiplication and division ///////////
/////////////////////////////////////////////////////////////////

// rank when sorting a commutativ expression
let expressionSortRank e1 =
    match e1 with
    | Neg _ -> 1
    | N _ -> 2
    | X _ -> 3
    | Add _ -> 4
    | Sub _ -> 5
    | Mul _ -> 6
    | Div _ -> 7
 
// sorts the commutativ list
let rec sort l = List.sortBy (fun e -> expressionSortRank e) l


// determines the sign of a commutativ list
let rec signList l s =
    match l with
    | [] -> s
    | Neg _::tail -> signList tail (-1*s)
    | _::tail -> signList tail s


// flattens the tree to a commutativ list
let rec flatTree e =
    match e with 
    | N _ | X _ | Add _ | Sub _ -> [e]
    | Neg a -> Neg (N one) :: flatTree a
    | Mul (a, b) -> flatTree a @ flatTree b
    | Div (N a, N b) -> [N (a / b)]
    | Div (Neg a, b) | Div (a, Neg b) -> Neg (N one) :: flatTree (Div (a, b))
    | Div (a, N b) -> N (one / b) :: flatTree a
    | Div (a, b) -> Div (N one, b) :: flatTree a 


// multiplys all Div elements in a commutativ list
let rec mulDivElements l =
    // division will always be in the end of a sorted list, and numarator will be 1
    match l with
    | [] -> []
    | Div (_, b) :: Div (_, c) :: tail -> mulDivElements (Div(N one, Mul(b, c)) :: tail)
    | x::tail -> x :: mulDivElements tail

// removes a element from a list
let rec removeElem e l =
    match  l with
    | [] -> []
    | x::tail when x = e -> tail
    | x::tail -> x :: removeElem e tail

// reduces a commutativ list
let rec reduce l =
    let sorted = divCancelling (sort l)
    if signList sorted 1 > 0 then rebuildTree sorted else Neg (rebuildTree sorted)

// initiates the division cancelling
and divCancelling l = 
    // printfn "DivCancelling: %A" l
    match List.rev l with
    | [] -> l
    | Div(_, b)::tail -> 
                        let (numerator, denominator) = cancelEquality  (List.rev tail) (sort (flatTree b))
                        sort (flatTree (reduce numerator / reduce denominator))
    | _ -> l

// cancels out equal elements in the numerator and denominator
and cancelEquality nu de =
    // printfn "cancelEquality: %A / %A" nu de
    match nu with
    | [] -> ([], de)
    | n::ntail when List.contains n de ->  cancelEquality ntail (removeElem n de)
    | n::ntail -> let (numerator, denominator) = cancelEquality ntail de
                  (n::numerator, denominator)
                


// rebuilds a commutativ list to a tree
and rebuildTree l =
    match l with
    | [] -> N one
    | Neg (N a)::tail when Number.isOne a -> rebuildTree tail
    | Neg a::tail -> rebuildTree (a::tail)
    | N a::N b::tail -> rebuildTree (N (a * b) :: tail)
    | N a :: Add(b, c) :: tail -> rebuildTree (Add(reduce (flatTree (Mul(N a, b))), reduce (flatTree (Mul(N a, c)))) :: tail)
    | N a :: Sub(b, c) :: tail -> rebuildTree (Sub(reduce (flatTree (Mul(N a, b))), reduce (flatTree (Mul(N a, c)))) :: tail)
    | a::tail -> a * rebuildTree tail


// applies the commutativ rules to a tree    
let applyCommutative e:Expr<Number> =
    match e with
    | Mul _ | Div _ -> reduce (flatTree e)
    | _ -> e
