module TreeGenerator
// #r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
// #load "modules/rantionalAndComplex.fs"
// #load "modules/Number.fs"
// #load "modules/Expression.fs"
open Number
open Expression

type Associative = | Left | Right
type Operator = char * int * Associative
type Symbol =
    | Operand of char
    | Operator of Operator
    | Konstant of int

type OperatorList = Operator list


let rec infixToSymbolList s =
    mapToSymbol (Seq.toList s) true false
and mapToSymbol l allowUnary allowOperator=
    //printfn "%A-%A-%A" l allowUnary allowOperator
    match l with
    | [] -> []
    | x::tail when x = ' ' -> mapToSymbol tail allowUnary allowOperator
    | x::tail when allowOperator && x = '/' || x = '*' -> Operator (x, 2, Left)::mapToSymbol tail false false 
    | x::tail when allowOperator && (x = '+' || (x = '-' && not allowUnary)) -> Operator (x, 1, Left)::mapToSymbol tail false false
    | x::tail when  (x = '-' && allowUnary) -> Operator ('~', 2, Right)::mapToSymbol tail true false
    | x::tail when x = '(' || x = ')' -> Operator (x, -1, Left)::mapToSymbol tail true true
    | x::_ when System.Char.IsDigit(x) -> 
        let (k, tail) = foundInt l ""
        Konstant (int k):: mapToSymbol tail false true
    | x::tail when System.Char.IsLetter x -> Operand x::mapToSymbol tail false true
    | x::_ -> failwith ("Invalid syntax at: " + string x) 
and foundInt l s =
    match l with
    | x::tail when System.Char.IsDigit(x) -> foundInt tail (s + string x)
    | _ -> (s, l)



let popPostfixStack op postfix =
    //printfn "%A :op %A :postix" op postfix 
    match op, postfix with
    | x, e1::e2::tail when x = '+' -> Add(e2, e1)::tail
    | x, e1::e2::tail when x = '-' -> Sub(e2, e1)::tail
    | x, e1::e2::tail when x = '*' -> Mul(e2, e1)::tail
    | x, e1::e2::tail when x = '/' -> Div(e2, e1)::tail
    | x, e::tail when x = '~' -> Neg(e)::tail
    | x, _ when x = '(' -> postfix
    |_,_ -> failwith "match not found"


let rec generateExpresion c (stack:OperatorList) postfix =
    match c, stack with
    | [], [] -> postfix
    | [], (s,_,_)::stack_tail  -> generateExpresion c stack_tail (popPostfixStack s postfix)  
    | Operand x :: tail, _ -> generateExpresion tail stack (X x::postfix)
    | Konstant x :: tail, _ -> generateExpresion tail stack (N (Int x)::postfix)
    | Operator (x, prec, lr)::tail, _ 
        when x = '(' 
        -> generateExpresion tail ((x, prec, lr)::stack) postfix
    | Operator (x, _, _)::tail, _
        when x = ')' 
        ->  match stack with
            | [] -> generateExpresion tail stack postfix
            | (s,_,_)::stack_tail 
                when s = '(' 
                -> generateExpresion tail stack_tail postfix
            | (s,_,_)::stack_tail 
                -> generateExpresion c stack_tail (popPostfixStack s postfix)
    | Operator e::tail, [] -> generateExpresion tail (e::stack) postfix
    | Operator e::tail, s::stack_tail 
        ->  match e, s with
            | (x, precX, lr), (y, precY, _) 
                when precX < precY || (precX = precY && lr = Left)
                -> generateExpresion c stack_tail (popPostfixStack y postfix)
            | _, _ -> generateExpresion tail (e::stack) postfix


let tree s = 
    match generateExpresion (infixToSymbolList s) [] [] with
    | [] -> failwith "Tree is empty" 
    |tree::_ -> tree


// uses modified inorder traversal to generate infix string from expression tree
let rec ExpressionToInfix e p =
    match e with
    | N a -> toString a
    | X a -> string a
    | Neg a -> "-" + ExpressionToInfix a true
    | Add(a, b) when p -> "(" + (ExpressionToInfix a false) + "+" + (ExpressionToInfix b false) + ")"
    | Add(a, b) -> (ExpressionToInfix a false) + "+" + (ExpressionToInfix b false)
    | Sub(a, b) when p -> "(" + (ExpressionToInfix a false) + "-" + (ExpressionToInfix b false) + ")"
    | Sub(a, b) -> (ExpressionToInfix a false) + "-" + (ExpressionToInfix b false)
    | Mul(a, b) -> (ExpressionToInfix a true) + "*" + (ExpressionToInfix b true) 
    | Div(a, b) -> (ExpressionToInfix a true)  + "/" + (ExpressionToInfix b true)