(*
    Code is largely based on slides from 02257 Applied Functional Programming
    about property-based testing.
*)
#r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
#r "nuget: FsCheck"
#load "SymbolicManipolation.fsx"
open FsCheck
open rational
open Number
open Expression


// generate a random natural abs( number ) between 1 and 100
let noneZeroGen = 
    Gen.oneof [ 
        Gen.choose(1, 10) ;
        Gen.choose(-10, -1)]

// picks a random variable from a list of variables
let varGen xlist = 
    gen { let! i = Gen.choose(0, List.length xlist - 1)
        return xlist.[i] }

// generate a random Number
let numberGen =
    Gen.oneof [
        Gen.map2 (fun x y -> Rational (make(x, y))) (Gen.choose(-10, 10)) noneZeroGen;
        Gen.map (fun x -> Int x) (Gen.choose(-10, 10))]

// generate a random Number (a Expresion leaf)
let numberInExprGen = 
    Gen.oneof [
        Gen.map2 (fun x y -> N (Rational (make(x, y)))) (Gen.choose(-10, 10)) noneZeroGen ;
        Gen.map (fun x -> N (Int x)) (Gen.choose(-10, 10))]

// generate a random variable (a Expresion leaf)
let XGen xlist = Gen.map X (varGen xlist)

// generate a random leaf
let leafGen xlist =
    if xlist <> [] then
        Gen.oneof [numberInExprGen; XGen xlist]
    else
        numberInExprGen

// generate a sequence of characters
let charsSeqGen c1 c2 = seq { for c in c1 .. c2 do
                                yield gen { return c} }

// pick a random character from a sequence of characters
let charGen = gen { return! Gen.oneof (charsSeqGen 'A' 'Z')}

// generate a random environment (map)
let smallEnvGen =
    gen { 
        let! i = Gen.choose (0, 5)
        let! xlist = Gen.listOfLength i charGen
        let! ns = Gen.listOfLength i numberGen
        return (Map.ofList (List.zip xlist ns), xlist) }

// generate a random expression
// let expr charlist n = 
//     let rec exprGen xlist n = 
//         if n = 0 then
//             leafGen xlist
//         else
//             Gen.oneof [
//                 leafGen xlist; // leaf occurs twice becourse leaf is X or N giving the same probability for each expression 
//                 leafGen xlist;
//                 Gen.map2 (fun x y -> Add (x, y)) (exprGen xlist (n/2)) (exprGen xlist (n/2));
//                 Gen.map2 (fun x y -> Mul (x, y)) (exprGen xlist (n/2)) (exprGen xlist (n/2));
//                 Gen.map2 (fun x y -> Div (x, y)) (exprGen xlist (n/2)) (exprGen xlist (n/2));
//                 Gen.map2 (fun x y -> Sub (x, y)) (exprGen xlist (n/2)) (exprGen xlist (n/2));            
//                 Gen.map (fun x -> Neg x) (exprGen xlist (n/2))]
//     Gen.sized (exprGen charlist)
let rec exprGen xlist n = 
        if n = 0 then
            leafGen xlist
        else
            Gen.oneof [
                leafGen xlist; // leaf occurs twice becourse leaf is X or N giving the same probability for each expression 
                leafGen xlist;
                Gen.map2 (fun x y -> Add (x, y)) (exprGen xlist (n/2)) (exprGen xlist (n/2));
                Gen.map2 (fun x y -> Mul (x, y)) (exprGen xlist (n/2)) (exprGen xlist (n/2));
                Gen.map2 (fun x y -> Div (x, y)) (exprGen xlist (n/2)) (exprGen xlist (n/2));
                Gen.map2 (fun x y -> Sub (x, y)) (exprGen xlist (n/2)) (exprGen xlist (n/2));            
                Gen.map (fun x -> Neg x) (exprGen xlist (n/2))]

type ExprWithEnv = Expr<Number> * Map<char, Number>
type SmallEnv = Map<char, Number> * char list

type SmallEnvGen =
    static member SmallEnv() =
        {new Arbitrary<SmallEnv>() with
            override _.Generator = smallEnvGen
            override _.Shrinker _ = Seq.empty}

Arb.register<SmallEnvGen>()


let compareSimpExpr env (e:Expr<Number>) =
    // printfn "Expression: %A" e
    eval (SymbolicManipolation.simplifyExpr e) env  = eval e env


let simpEqualEval se = 
    try
        let (env, xlist) = se
        let expr = Gen.sample 1 1 (exprGen xlist 10) |> List.head
        printfn "Expression: %A" expr
        if expr |> compareSimpExpr env then 1 else 0
    with
        | :? System.DivideByZeroException as ex ->
            printfn "DivideByZeroException: %A" ex
            2

let simpPBT (se:SmallEnv) =
    let result = simpEqualEval se
    (result = 1 || result = 2)
    |> Prop.classify (result = 1) "Equal"
    |> Prop.classify (result = 2) "DivideByZeroExceptions"


let _ = Check.Quick simpPBT

