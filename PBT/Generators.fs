module Generators

// #r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
// #r "nuget: FsCheck"
// #load "../SymbolicManipolation.fsx"
// #load "../modules/Matrix.fs"
open FsCheck
open rantionalAndComplex
open Number
open Expression
open Matrix
// open SymbolicManipolation



let max = 10
let min = -10

// generate a random natural abs( number ) between 1 and 100
let noneZeroGen = 
    Gen.oneof [ 
        Gen.choose(1, max) ;
        Gen.choose(min, -1)]

// picks a random variable from a list of variables
let varGen xlist = 
    gen { let! i = Gen.choose(0, List.length xlist - 1)
        return xlist.[i] }

// generate a random Number
let numberGen =
    Gen.oneof [
        Gen.map2 (fun x y -> Rational (makeR(x, y))) (Gen.choose(min, max)) noneZeroGen;
        Gen.map (fun x -> Int x) (Gen.choose(min, max));
        Gen.map4 (fun a b c d -> Complex (makeC (makeR(a, b), makeR(c, d)))) (Gen.choose(min, max)) noneZeroGen (Gen.choose(min, max)) noneZeroGen]

// generate a random Number (a Expresion leaf)
let numberInExprGen = 
    Gen.map (fun x -> N x) numberGen

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

type SmallEnv = Map<char, Number> * char list
type SmallEnvGen =
    static member SmallEnv() =
        {new Arbitrary<SmallEnv>() with
            override _.Generator = smallEnvGen
            override _.Shrinker _ = Seq.empty}

// Arb.register<SmallEnvGen>()

/////////////////////////////////
/// Matrix Generators ///////////
/////////////////////////////////


// A generator to get small ints
type SmallInt = SmallInt of int

let smallIntGen = Gen.map (fun x -> SmallInt x) (Gen.choose (1, max))

// generate a random vector of length n
let vectorGen n =
    Gen.listOfLength n numberGen |> Gen.map (fun x -> vector x)

let sampleTwoSmallInts = 
    let s = Gen.sample 1 2 smallIntGen
    (s.[0], s.[1])

let matrixGen =
    let (SmallInt row, SmallInt col) = sampleTwoSmallInts
    Gen.listOfLength col (vectorGen row) |> Gen.map (fun x -> matrix x)


type MaxtrixGen =
    static member Matrix() =
        {new Arbitrary<Matrix>() with
            override _.Generator = matrixGen
            override _.Shrinker _ = Seq.empty}

// Arb.register<MaxtrixGen>()

let getSmallInt = 
    Gen.sample 1 1 smallIntGen |> List.head
    

type VectorGen =
    static member Vector() =
        {new Arbitrary<Vector>() with
            override _.Generator = (fun (SmallInt x) -> vectorGen x) getSmallInt
            override _.Shrinker _ = Seq.empty}

// Arb.register<VectorGen>()


type NumberGen =
    static member Number() =
        {new Arbitrary<Number>() with
            override _.Generator = numberGen
            override _.Shrinker _ = Seq.empty}

// Arb.register<NumberGen>()

/////////////////////////////////
/// Properties //////////////////
/////////////////////////////////


// Property: The addition of vectors is associative Theorem 7.2
let vectorAss m =
    sumRows m = sumRows (flip m)

let vectorScalarAss (m:Matrix) (n1:Number) (n2:Number) =
    n1 * (n2 * m) = (n1 * n2) * m

// test c * (v1 + ..+  vn) = c*v1 + .. + c*v2
let vectorAssCom m n =
    n * (sumRows m) = sumRows (n * m)


