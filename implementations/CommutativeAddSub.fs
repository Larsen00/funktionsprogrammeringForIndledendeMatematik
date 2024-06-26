module CommutativeAddSub
open Expression
open Number


// rank when sorting a commutative expression
let expressionSortRank e1 =
    match e1 with
    | N _ -> 1
    | Neg (N _) -> 2
    | X _ -> failwith "Variables should not be in the list" // all veriables are multiplied with 1
    | Add _ -> failwith "Addition should not be in the list" // all addition should be reduced
    | Sub _ -> failwith "Subtraction should not be in the list" // all subtraction should be reduced    
    | Mul _ -> 6
    | Div _ -> 7
    | Neg _ -> 8

// Flattens a expression tree with respect to addition and subtraction
let rec flatTree e =
    match e with
    | Add (a, b) -> flatTree a @ flatTree b
    | Sub (a, b) -> flatTree a @ flatTree (Neg b)
    | Neg (Add(a, b)) -> flatTree (Neg a) @ flatTree (Neg b)
    | Neg (Sub(a, b)) -> flatTree (Neg a) @ flatTree b
    | X a -> [Mul(N one, X a)]
    | Div (a, b) -> [a / b]
    | Neg (Neg a) -> flatTree a
    | N _ | Div _ | Mul _ | Neg _-> [e]



let rec sort l = List.sortBy (fun e -> expressionSortRank e) l

// Reduces a sorted commutative list for addition
let rec reduceNumbers l =
    // printfn "rn %A" l
    match l with
    | [] -> []
    | N a :: N b :: tail -> reduceNumbers (N a + N b :: tail)
    | N a :: Neg (N b) :: tail  -> reduceNumbers (N a - N b :: tail)
    | Neg (N a) :: Neg (N b) :: tail -> reduceNumbers (Neg (N a + N b) :: tail)
    | Neg (N a) :: tail -> Neg (N a) :: tail
    | N a :: tail -> N a :: tail
    | _ -> l

// given a list of commutative expressions, and a variable, sums the corefficients of the variable
let rec sumInstancesOfVarible x l =
    match x, l with
    | _, [] -> ([] ,x)
    | Mul(N n1, X x1), Mul(N n2, X x2) :: tail 
    | Mul(N n1, X x1), Mul(X x2, N n2) :: tail
    | Mul(X x1, N n1), Mul(N n2, X x2) :: tail
    | Mul(X x1, N n1), Mul(X x2, N n2) :: tail
        when x1 = x2    -> sumInstancesOfVarible (Mul(N (n1 + n2), X x1)) tail
    | _, head :: tail   -> 
                        let (l_new, x_new) = sumInstancesOfVarible x tail
                        (head :: l_new, x_new)
    



let rec reduceVariables l = 
    match l with
    | [] -> []
    | Mul(N a, X b) :: tail
    | Mul(X b, N a) :: tail 
        -> 
        let (l_new, x_new) = sumInstancesOfVarible (Mul(N a, X b)) tail
        x_new :: reduceVariables l_new
    | head :: tail -> head :: reduceVariables tail
    
let rec rebuldTree l =
    // printfn "rt %A" l
    match l with
    | [] -> N zero
    | Neg x::tail -> (rebuldTree tail) - x
    | Mul (a, b)::tail -> (rebuldTree tail) + (a * b)
    | Div (a, b)::tail -> (rebuldTree tail) + (a / b)
    | x::tail -> rebuldTree tail + x

let applyCommutative e =
    match e with 
    | Sub _ | Add _ -> flatTree e |> sort |> reduceNumbers |> reduceVariables |> rebuldTree
    | _ -> e

