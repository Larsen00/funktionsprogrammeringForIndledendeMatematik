module Generators
open FsCheck
open rational
open complex
open Number
open Expression
open Matrix


let max = 3
let min = -3

// generate a random natural abs( number ) 
let noneZeroGen = 
    Gen.oneof [ 
        Gen.choose(1, max) ;
        Gen.choose(min, -1)]

// generate a random Number
let numberGen =
    Gen.oneof [
        Gen.map2 (fun x y -> newRational(x, y) |> Rational |> tryReduce ) (Gen.choose(min, max)) noneZeroGen;
        Gen.map (fun x -> Int x) (Gen.choose(min, max));
        Gen.map4 (fun a b c d -> newComplex (newRational(a, b), newRational(c, d)) |> Complex |> tryReduce ) (Gen.choose(min, max)) noneZeroGen (Gen.choose(min, max)) noneZeroGen]

// generate a random Number (a Expresion leaf)
let numberInExprGen = 
    Gen.map (fun x -> N x) numberGen

// picks a random variable from a list of variables
let randomListElement xlist = 
    gen { let! i = Gen.choose(0, List.length xlist - 1)
        return xlist.[i] }

// generate a random variable (a Expresion leaf)
let variableGen xlist = Gen.map X (randomListElement xlist)

// generate a random leaf
let leafGen xlist =
    if xlist <> [] then
        Gen.oneof [numberInExprGen; variableGen xlist]
    else
        numberInExprGen

// generate a random leaf with only integer values
let onlyIntleafGen xlist :Gen<Expr<Number>> = 
    if xlist <> [] then
        Gen.oneof [Gen.map (fun x -> N <| Int x) (Gen.choose(-10, 10)); variableGen xlist]
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


let getDiagonalMatrixGen maxRows =
    gen { 
        let! m = Gen.choose(2, maxRows)
        let! n = Gen.choose(m, m + 3)   
        return fullrankedDiagonalMatrix m n }
        

let getIndependentBacisGen =
    gen { 
        let! A = getDiagonalMatrixGen 5
        let! numberOfOperations = Gen.choose(1, 10)
        let! span = multipleRowOperationsGen A numberOfOperations
        return span |> transposeMatrix  }

type independentBacisMatrix = Matrix
type independentBacisMatrixGen =
    static member independentBacisMatrix() =
        {new Arbitrary<Matrix>() with 
            override _.Generator = getIndependentBacisGen
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


let gramSchmidtIsOrthogonal (m:independentBacisMatrix) =
    let res =
        try 
            let um = orthogonalBacis m
            if  isOrthogonalBacis um && hasSameSpan m um then 1 else 0
        with
            | :? System.DivideByZeroException as _ -> 2
            | :? System.OverflowException as _ -> 3

    (res = 1 || res = 2 || res = 3)
    |> Prop.classify (res = 1) "PropertyHolds"
    |> Prop.classify (res = 2) "DivideByZeroExceptions"
    |> Prop.classify (res = 3) "OverflowException"

let independentBacisMatrixHasFullRank (m:independentBacisMatrix) =
    let res =
        try 
            if hasFullRank m then 1 else 0
        with
            | :? System.OverflowException -> 2
    (res = 1 || res = 2)
    |> Prop.classify (res = 1) "PropertyHolds"
    |> Prop.classify (res = 2) "OverflowException"

