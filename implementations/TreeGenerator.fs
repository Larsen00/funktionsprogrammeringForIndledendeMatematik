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

// Converts a infix string to a list of tokens
let rec infixToTokenList s =
    mapToToken (Seq.toList s) true false

// maps a token to its corresponding token type
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

// Allowing more than one digit in a number
and foundInt l s =
    match l with
    | x::tail when System.Char.IsDigit(x) -> foundInt tail (s + string x)
    | _ -> (s, l)


// pops the last operator from the stack and adds it to the prefix list
let popprefixStack op prefix =
    match op, prefix with
    | x, e1::e2::tail when x = '+' -> Add(e2, e1)::tail
    | x, e1::e2::tail when x = '-' -> Sub(e2, e1)::tail
    | x, e1::e2::tail when x = '*' -> Mul(e2, e1)::tail
    | x, e1::e2::tail when x = '/' -> Div(e2, e1)::tail
    | x, e::tail when x = '~' -> Neg(e)::tail
    | x, _ when x = '(' -> prefix
    |_,_ -> failwith "match not found"

// Runs the algorithm to generate the expression tree
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

// Converts a infix string to a expression tree
let tree s =
    match generateExpresion (infixToTokenList s) [] [] with
    | [] -> failwith "Tree is empty" 
    |tree::_ -> tree

// Adds parenthesis to a string if a boolean is true
let parenthesis b f = 
    if b then "(" + f + ")" else f

// Converts a expression tree to a infix string
let rec etf e p =
    match e with
    | N a when not <| isInt a -> toString a |> parenthesis p
    | N a -> toString a
    | X a -> string a
    | Neg a ->  "-" + etf a (not p) |> parenthesis p
    | Add(a, b) -> etf a false + "+" + etf b false |> parenthesis p
    | Sub(a, b) -> etf a false + "-" + etf b true |> parenthesis p
    | Mul(a, b) -> etf a true + "*" + etf b true |> parenthesis p
    | Div(a, b) -> etf a true + "/" + etf b true |> parenthesis p

// Converts a expression tree to a infix string
let infixExpression e = etf e false