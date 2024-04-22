module Generators

// #r "C:/Users/jonas/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/main/bin/Release/net7.0/main.dll"
// #r "nuget: FsCheck"
// #load "../SymbolicManipolation.fsx"
// #load "../modules/Matrix.fs"
open FsCheck
// open rantionalAndComplex
open rational
open complex
open Number
open Expression
open Matrix
// open SymbolicManipolation



let max = 3
let min = -3

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
        Gen.map2 (fun x y -> Rational (newRational(x, y))) (Gen.choose(min, max)) noneZeroGen;
        Gen.map (fun x -> Int x) (Gen.choose(min, max));
        Gen.map4 (fun a b c d -> Complex (newComplex (newRational(a, b), newRational(c, d)))) (Gen.choose(min, max)) noneZeroGen (Gen.choose(min, max)) noneZeroGen]

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

let onlyIntleafGen xlist :Gen<Expr<Number>> = 
    if xlist <> [] then
        Gen.oneof [Gen.map (fun x -> N <| Int x) (Gen.choose(-10, 10)); XGen xlist]
    else
        Gen.map (fun x -> N <| Int x) (Gen.choose(-10, 10))

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

let rec exprGen xlist n leafType = 
        if n = 0 then
            leafType xlist
        else
            Gen.oneof [
                leafType xlist; // leaf occurs twice becourse leaf is X or N giving the same probability for each expression 
                leafType xlist;
                Gen.map2 (fun x y -> Add (x, y)) (exprGen xlist (n/2) leafType) (exprGen xlist (n/2) leafType);
                Gen.map2 (fun x y -> Mul (x, y)) (exprGen xlist (n/2) leafType) (exprGen xlist (n/2) leafType);
                Gen.map2 (fun x y -> Div (x, y)) (exprGen xlist (n/2) leafType) (exprGen xlist (n/2) leafType);
                Gen.map2 (fun x y -> Sub (x, y)) (exprGen xlist (n/2) leafType) (exprGen xlist (n/2) leafType);            
                Gen.map (fun x -> Neg x) (exprGen xlist (n/2) leafType)]


type SmallEnv = Map<char, Number> * char list
type SmallEnvGen =
    static member SmallEnv() =
        {new Arbitrary<SmallEnv>() with
            override _.Generator = smallEnvGen
            override _.Shrinker _ = Seq.empty}

// Arb.register<SmallEnvGen>()

type NumberGen =
    static member Number() =
        {new Arbitrary<Number>() with
            override _.Generator = numberGen
            override _.Shrinker _ = Seq.empty}


/////////////////////////////////
/// Matrix Generators ///////////
/////////////////////////////////

// generate a random vector of length n
let vectorGen n =
    Gen.listOfLength n numberGen |> Gen.map (fun x -> vector x)

let matrixGen =
    gen {
        let! row = Gen.choose(1, 6)
        let! col = Gen.choose(1, 6)
        let! vectors = Gen.listOfLength col (vectorGen row)
        return matrix vectors
    }

type MaxtrixGen =
    static member Matrix() =
        {new Arbitrary<Matrix>() with
            override _.Generator = matrixGen
            override _.Shrinker _ = Seq.empty}



let getBacismatrixGen n =
    Gen.map (fun x -> standardBacis x) (Gen.choose (2, n))

let performRowOperationGen m =
    let (D(n, _)) = dimMatrix m
    gen { 
        let! i = Gen.choose(1, n)
        let! j = match i with
                    | 1 -> Gen.choose(2, n)
                    | _ when i = n -> Gen.choose(1, n-1)
                    | _ -> Gen.oneof [Gen.choose(1, i-1); Gen.choose(i+1, n)]
        let! a = numberGen
        return rowOperation i j a m }

let rec multipleRowOperationsGen m count =
    if count <= 0 then Gen.constant m
    else
        gen {
            let! newMatrix = performRowOperationGen m
            return! multipleRowOperationsGen newMatrix (count - 1)
        }

let getIndependetBacisGen =
    gen { 
        let! m = getBacismatrixGen 5
        let! numberOfOperations = Gen.choose(1, 10)
        let! span = multipleRowOperationsGen m numberOfOperations
        return span }

type IndependetBacis = Matrix
type IndependetBacisGen =
    static member IndependetBacis() =
        {new Arbitrary<Matrix>() with
            override _.Generator = getIndependetBacisGen
            override _.Shrinker _ = Seq.empty}


let getDiagonalMatrixGen maxRows =
    gen { 
        let! n = Gen.choose(2, maxRows)
        let! m = Gen.choose(n, n + 3)   
        return fullrankedDiagonalMatrix n m }
        

let getFullRankedMatrixGen =
    gen { 
        let! m = getDiagonalMatrixGen 5
        let! numberOfOperations = Gen.choose(1, 10)
        let! span = multipleRowOperationsGen m numberOfOperations
        return span }

type fullRankedMatrix = Matrix
type fullRankedMatrixGen =
    static member fullRankedDiagonalMatrix() =
        {new Arbitrary<Matrix>() with 
            override _.Generator = getFullRankedMatrixGen
            override _.Shrinker _ = Seq.empty}
            

/////////////////////////////////
/// Properties //////////////////
/////////////////////////////////


// Property: The addition of vectors is commutative Theorem 7.2
let vectorCom m =
    sumRows m = sumRows (flip m)

let vectorScalarAss (m:Matrix) (n1:Number) (n2:Number) =
    n1 * (n2 * m) = (n1 * n2) * m

// test c * (v1 + ..+  vn) = c*v1 + .. + c*vn
let vectorAssCom m (c:Number) =
    c * (sumRows m) = sumRows (c * m)

let transposeTwice (m:Matrix) =
    transposeMatrix m |> transposeMatrix = m

// An independtent set of vectors is orthogonal after the Gram-Schmidt process
let gramSchmidtIsOrthogonal (m:IndependetBacis) =
    let res =
        try 
            if orthogonalBacis m |> isOrthogonalBacis then 1 else 0
        with
            | :? System.OverflowException -> 2
    (res = 1 || res = 2)
    |> Prop.classify (res = 1) "PropertyHolds"
    |> Prop.classify (res = 2) "OverflowException"

let gramSchmidtIsOrthogonal2 (m:fullRankedMatrix) =
    let res =
        try 

            printfn "\nNEW TEST"
            let tm = transposeMatrix m
            printfn "%A" (stringMatrix tm)
            let um = orthogonalBacis tm
            if  isOrthogonalBacis um && hasSameSpan tm um then 1 else 0
        with
            | :? System.OverflowException -> 2
    (res = 1 || res = 2)
    |> Prop.classify (res = 1) "PropertyHolds"
    |> Prop.classify (res = 2) "OverflowException"

let fullRankedMatrixIsFullRanked (m:IndependetBacis) =
    let res =
        try 
            if hasFullRank m then 1 else 0
        with
            | :? System.OverflowException -> 2
    (res = 1 || res = 2)
    |> Prop.classify (res = 1) "PropertyHolds"
    |> Prop.classify (res = 2) "OverflowException"

