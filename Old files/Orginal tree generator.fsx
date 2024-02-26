(*
    Sources:
    https://www.geeksforgeeks.org/convert-infix-expression-to-postfix-expression/

    Limits:
    Virker kun med ints 
    Må ikke opløfte i en variable og diff
    
*)

#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
open rational

// Expression
type Associative = | L | R
type Operator = char * int * Associative
type Symbol =
    | Operand of char
    | Operator of Operator
    | Konstant of int

type OperatorList = Operator list

type E = 
                | X of char
                | N of int
                | Add of E*E 
                | Neg of E
                | Sub of E*E
                | Mul of E*E 
                | Div of E*E
                | Sq of E*E 



let test_infix = "A + B"

let test = [Operator ('(', -1, L); Operand 'A'; Operator ('+', 4, L) ; Operand 'B'; Operator (')', -1, L)] 


let rec infixToSymbolList s =
    mapToSymbol (Seq.toList s) true false
and mapToSymbol l allowUnary allowOperator=
    //printfn "%A-%A-%A" l allowUnary allowOperator
    match l with
    | [] -> []
    | x::tail when allowOperator && x = ' ' -> mapToSymbol tail allowUnary allowOperator
    | x::tail when allowOperator && x = '^' -> Operator (x, 3, R)::mapToSymbol tail false false
    | x::tail when allowOperator && x = '/' || x = '*' -> Operator (x, 2, L)::mapToSymbol tail false false 
    | x::tail when allowOperator && (x = '+' || (x = '-' && not allowUnary)) -> Operator (x, 1, L)::mapToSymbol tail false false
    | x::tail when  (x = '-' && allowUnary) -> Operator ('~', 2, R)::mapToSymbol tail true false
    | x::tail when x = '(' || x = ')' -> Operator (x, -1, L)::mapToSymbol tail true true
    | x::_ when System.Char.IsDigit(x) -> 
        let (k, tail) = foundInt l ""
        Konstant (int k):: mapToSymbol tail false true
    | x::tail when System.Char.IsLetter x -> Operand x::mapToSymbol tail false true
    | x::_ -> failwith ("Invalid syntax at: " + string x) 
and foundInt l s =
    match l with
    | x::tail when System.Char.IsDigit(x) -> foundInt tail (s + string x)
    | _ -> (s, l)



let eval op postfix =
    //printfn "%A :op %A :postix" op postfix 
    match op, postfix with
    | x, e1::e2::tail when x = '+' -> Add(e2, e1)::tail
    | x, e1::e2::tail when x = '-' -> Sub(e2, e1)::tail
    | x, e1::e2::tail when x = '*' -> Mul(e2, e1)::tail
    | x, e1::e2::tail when x = '/' -> Div(e2, e1)::tail
    | x, e1::e2::tail when x = '^' -> Sq(e2, e1)::tail
    | x, e::tail when x = '~' -> Neg(e)::tail
    | x, _ when x = '(' -> postfix
    |_,_ -> failwith "match not found"


let rec convert c (stack:OperatorList) postfix =
    //printfn "\n%A" c
    //printfn "%A" stack
    //printfn "%A" postfix
    match c, stack with
    | [], [] -> postfix
    | [], (s,_,_)::stack_tail  -> convert c stack_tail (eval s postfix)  
    | Operand x :: tail, _ -> convert tail stack (X x::postfix)
    | Konstant x :: tail, _ -> convert tail stack (N x::postfix)
    | Operator (x, prec, lr)::tail, _ 
        when x = '(' 
        -> convert tail ((x, prec, lr)::stack) postfix

    | Operator (x, _, _)::tail, _
        when x = ')' 
        ->  match stack with
            | [] -> convert tail stack postfix
            | (s,_,_)::stack_tail 
                when s = '(' 
                -> convert tail stack_tail postfix
            | (s,_,_)::stack_tail 
                -> convert c stack_tail (eval s postfix)
    
    | Operator e::tail, [] -> convert tail (e::stack) postfix
    | Operator e::tail, s::stack_tail 
        ->  match e, s with
            | (x, precX, lr), (y, precY, _) 
                when precX < precY || (precX = precY && lr = L)
                -> convert c stack_tail (eval y postfix)
            | _, _ -> convert tail (e::stack) postfix



// printfn "%A" (convert test [] []) 
// printfn "%A" (convert (infixToSymbolList "(300+A+B)") [] [])
// printfn "%A" (convert (infixToSymbolList "(10^30)*X+B") [] [])
// printfn "%A" (convert (infixToSymbolList "(X+2)^2*45") [] []) 
// printfn "%A" (convert (infixToSymbolList "-(X+2)^2*45") [] [])
// printfn "%A" (convert (infixToSymbolList "-(X+2)^(-2)*45") [] [])


// printfn "DIFF"
let rec diff f dx = 
    match f with
    | X a when a = dx -> N 1
    | X _ -> N 0
    | N a -> N 0
    | Neg a -> Neg (diff a dx)
    | Add(a, b) -> Add(diff a dx, diff b dx)
    | Sub(a, b) -> Sub(diff a dx, diff b dx)
    | Mul(a, b) -> Add(Mul(diff a dx, b), Mul(a, diff b dx))
    | Sq(a, b) -> Sq(Mul(b, a), Sub(b, N 1))



let f::_ = (convert (infixToSymbolList "2*x") [] [])
// printfn "%A" (diff f 'x')
let g::_ = (convert (infixToSymbolList "-(X+2)^(-2)*45") [] [])
// printfn "%A" (diff g 'x')


