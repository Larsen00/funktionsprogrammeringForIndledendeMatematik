module TreeGenerator
open Number
open Expression

type Associative = | Left | Right
type Precedence = int
type Operator = char * Precedence * Associative
type Token =
    | Operand of char
    | Operator of Operator
    | Konstant of int
type OperatorList = Operator list


let rec infixToTokenList s =
    mapToToken (Seq.toList s) true false
and mapToToken l allowUnary allowOperator=
    match l with
    | [] -> []
    | x::tail when x = ' ' -> mapToToken tail allowUnary allowOperator
    | x::tail when allowOperator && x = '/' || x = '*' -> Operator (x, 2, Left)::mapToToken tail false false 
    | x::tail when allowOperator && (x = '+' || (x = '-' && not allowUnary)) -> Operator (x, 1, Left)::mapToToken tail false false
    | x::tail when  (x = '-' && allowUnary) -> Operator ('~', 2, Right)::mapToToken tail true false
    | x::tail when x = '(' -> Operator (x, -1, Left)::mapToToken tail true true
    | x::tail when x = ')' -> Operator (x, -1, Left)::mapToToken tail false true
    | x::_ when System.Char.IsDigit(x) -> 
        let (k, tail) = foundInt l ""
        Konstant (int k):: mapToToken tail false true
    | x::tail when System.Char.IsLetter x -> Operand x::mapToToken tail false true
    | x::_ -> failwith ("Invalid syntax at: " + string x) 
and foundInt l s =
    match l with
    | x::tail when System.Char.IsDigit(x) -> foundInt tail (s + string x)
    | _ -> (s, l)



let popprefixStack op prefix =
    match op, prefix with
    | x, e1::e2::tail when x = '+' -> Add(e2, e1)::tail
    | x, e1::e2::tail when x = '-' -> Sub(e2, e1)::tail
    | x, e1::e2::tail when x = '*' -> Mul(e2, e1)::tail
    | x, e1::e2::tail when x = '/' -> Div(e2, e1)::tail
    | x, e::tail when x = '~' -> Neg(e)::tail
    | x, _ when x = '(' -> prefix
    |_,_ -> failwith "match not found"


let rec generateExpresion c (stack:OperatorList) prefix =
    match c, stack with
    | [], [] -> prefix
    | [], (s,_,_)::stack_tail  -> generateExpresion c stack_tail (popprefixStack s prefix)  
    | Operand x :: tail, _ -> generateExpresion tail stack (X x::prefix)
    | Konstant x :: tail, _ -> generateExpresion tail stack (N (Int x)::prefix)
    | Operator (x, prec, lr)::tail, _ 
        when x = '(' 
        -> generateExpresion tail ((x, prec, lr)::stack) prefix
    | Operator (x, _, _)::tail, _
        when x = ')' 
        ->  match stack with
            | [] -> generateExpresion tail stack prefix
            | (s,_,_)::stack_tail 
                when s = '(' 
                -> generateExpresion tail stack_tail prefix
            | (s,_,_)::stack_tail 
                -> generateExpresion c stack_tail (popprefixStack s prefix)
    | Operator e::tail, [] -> generateExpresion tail (e::stack) prefix
    | Operator e::tail, s::stack_tail 
        ->  match e, s with
            | (x, precX, lr), (y, precY, _) 
                when precX < precY || (precX = precY && lr = Left)
                -> generateExpresion c stack_tail (popprefixStack y prefix)
            | _, _ -> generateExpresion tail (e::stack) prefix


let tree s =
    match generateExpresion (infixToTokenList s) [] [] with
    | [] -> failwith "Tree is empty" 
    |tree::_ -> tree



let parenthesis b f = if b then "(" + f + ")" else f

// uses modified inorder traversal to generate infix string from expression tree
let rec etf e p =
    match e with
    | N a -> toString a
    | X a -> string a
    | Neg a -> "-" + etf a true |> parenthesis p
    | Add(a, b) -> parenthesis p <| etf a false + "+" + etf b false |> parenthesis p
    | Sub(a, b) -> parenthesis p <| etf a false + "-" + etf b true |> parenthesis p
    | Mul(a, b) -> parenthesis p <| etf a true + "*" + etf b true |> parenthesis p
    | Div(a, b) -> parenthesis p <| etf a true + "/" + etf b true |> parenthesis p


let infixExpression e = etf e false