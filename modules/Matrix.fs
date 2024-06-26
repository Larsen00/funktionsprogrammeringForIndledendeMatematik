module Matrix
open Number
open Expression
open SymbolicManipolation

// row or coloumn major ordere: default of a vector and matric is Column major order.
type Order = | R | C

// Rows x Cols
type Dimension  = D of int * int

// a "Normal" is C major and a Transposed one is R major
type Vector = V of list<Number> * Order
type Matrix = M of list<Vector> * Order

// Construct a vector
let vector nl = V (nl, C)

// The dimension of a vector
let dimVector (V(v, o)) = 
    let len = List.length v
    match o with
    | R -> D (1, len)
    | C -> D (len, 1)

// Makes sure that a matrix has the same major order as its vectors
let matrixValidMajor (M(m, o)) =
    match List.head m, o with
    | V (_, R), R -> true
    | V (_, C), C -> true
    | _, _ -> failwith "Matrix's vector's dont have same major order"

// The list length of a vector
let vectorLength (V(v, _)) = List.length v

// The list length of a matrix's vectors
let matrixVectorLength (M(m, _)) = 
    match m with
    | [] -> 0
    | x::_ -> vectorLength x

// The dimension of a matrix
let dimMatrix (M(vl, o)) =
    if vl = [] then D (0, 0)
    else
    let _ = matrixValidMajor (M(vl, o))
    let d1 = List.length vl
    let d2 = matrixVectorLength (M(vl, o))
    match o with
    | R -> D (d1, d2)
    | C -> D (d2, d1)

// Multiplication of a scalar and a vector
let scalarVector (n:Number) (V (nl, o)) = 
    V ((List.map (fun x -> x * n) nl), o)

// Multiplication of a scalar and a matrix
let scalarMatrix (M (vl, o)) n = 
    M ((List.map (fun x -> scalarVector n x) vl), o)

// Addision two vectors
let addVector x y =
    let (V (v1, o1)) = x
    let (V (v2, o2)) = y
    if o1 <> o2 
    then failwith "Vectors must have the same major order"
    elif dimVector x <> dimVector y 
    then failwith "Vectors must have the same dimension"
    else 
    V ((List.map2 (+) v1 v2), o1)

// Subtraction of two vectors
let subVector x y =
    scalarVector (-one) y |> addVector x 


// Construct a vector of n with length len
let vectorOf n len = V ((List.init len (fun _ -> n)), C)

// Construct a matrix of n with dimension d
let matrixOf n (D (r, c)) = M ((List.init c (fun _ -> vectorOf n r)), C)

// Transposes a vector
let transposeVector (V(v, o)) =
    match o with
    | R -> V(v, C)
    | C -> V(v, R)

// adds a dimension to a vector
let extendVector (V(v, o)) n =
    V(v @ [n], o)

// Extend a matrix with a number list
let extendMatrix (M(m, o)) nl =
    if m <> [] && nl <> [] && List.length nl <> matrixVectorLength (M(m, o)) then failwith "The list must have the same length as the matrix's vectors"
    elif nl = [] then M(m, o)
    else
    M (m @ [(V (nl, o))], o)

// extend a matrix with a matrix
let extendMatrixWithMatrix (M(m1, o1)) (M(m2, o2)) =
    if o1 <> o2 then failwith "Matrices must have the same major order"
    elif matrixVectorLength (M(m1, o1)) <> matrixVectorLength (M(m2, o2)) then failwith "Matrices must have the same dimension"
    else M (m1 @ m2, o1)  


// Alternates the Major order
let alternateOrder o =
    match o with
    | R -> C
    | C -> R

// Alternates the Major order of a matrix
let alternateOrderMatrix (M(m, o)) =
    M (m, alternateOrder o)

// The i'th number of a vector
let getVectorIthNumber i (V(v, _)) = v.[i]

// The i'th vector of a matrix
let getMatrixIthVector i (M(m, _)) = m.[i]

// replaces the i'th vector of a matrix
let replaceMatrixIthVector i (M(m, o)) v =
    M (m.[0..i-1] @ [v] @ m.[i+1..], o)

// pops a vector 
let seperateFistNumberFromVector (V(v, o)) = 
    (v.Head, V(v.Tail, o))

// first element of a vector
let headVector (V(v, _)) = List.head v

// Gives a vector of all the first elements of the vectors in a matrix
let rec firstElemetsVectors (M(m, o)) v_acc m_acc = 
    match m with
    | [] -> (v_acc, M(m_acc, o))
    | x::xs ->
            let (n, tail) = seperateFistNumberFromVector x
            firstElemetsVectors (M(xs, o)) (v_acc @ [n]) (m_acc @ [tail])

// Helper function for changeOrderMatrix
let rec chaingingOrderMatrix (M(m, o)) m_acc = 
    match matrixVectorLength (M(m, o)) with
    | 0  -> m_acc
    | _  -> 
        let (v, m_new) = firstElemetsVectors (M(m, o)) [] []
        extendMatrix m_acc v |> chaingingOrderMatrix m_new 

// Changes the order of a matrix
let changeOrderMatrix (M(m, o)) =
    alternateOrderMatrix (M([], o)) |> chaingingOrderMatrix (M(m, o)) 

// Makes sure that two matrices have the same major order, if diffrent then changes the order of the second matrix
let giveMatrixHaveSameOrder (M(_, o1)) (M(m2, o2)) = 
    if o1 <> o2 then changeOrderMatrix (M(m2, o2)) else M(m2, o2)

// Transposes a matrix
let transposeMatrix (M(vl, o)) =
    M(List.map (fun x -> transposeVector x) vl, alternateOrder o)

// Adds two matrices elemtwise
let addMatrix m1 m2 =
    let m3 = giveMatrixHaveSameOrder m1 m2
    if dimMatrix m1 <> dimMatrix m3 
    then failwith "addMatrix: Matrices must have the same dimension"
    else
    let (M(vl1, o)) = m1
    let (M(vl3, _)) = m3
    M (List.map2 addVector vl1 vl3, o)

// Construct a matrix
let matrix vl =
    let rec mc vl m =
        match vl with
        | [] -> m
        | (V(x, _))::xs -> mc xs (extendMatrix m x)
    mc vl (M([], C))

// Changes the order of af matrix to x
let correctOrder (M(m, o)) x =
    if o = x then M(m, o) else changeOrderMatrix (M(m, o))

// Boolean value of if a matrix has the correct order
let corectOrderCheck (M(_, o)) x =
    o = x

// Sum the rows of a matrix
let rec sumRows m = 
    if not <| corectOrderCheck m C 
    then sumRows <| correctOrder m C
    else
    let zeroVector = vectorOf zero <| matrixVectorLength m
    let (M(vl, _)) = m
    matrix [List.fold (addVector) zeroVector vl]



// matrix vector product  A.v = b - Definition 7.10
let rec matrixVectorProduct m v =
    let (D(rv, _)) = dimVector v
    let (D(_, cm)) = dimMatrix m
    if rv <> cm  
    then failwith "matrixVector: the number of columns of the matrix has to be the same as the number of entries in the vector."
    elif not <| corectOrderCheck m C then matrixVectorProduct (correctOrder m C) v
    else
    let (M(vl, _)) = m
    let (V(nl, _)) = v
    M (List.map2 (fun mc n -> scalarVector n mc) vl nl, C) 
    |> sumRows

// Converts a matrix to a vector if possible
let matrixToVector m =
    match dimMatrix m, m with
    | D(r, c), M(v::_, _) when r = 1 || c = 1 -> v
    | _, _ -> failwith "mactrixToVector: Matrix is not a vector"
    
// Multiplies two matrices - Definition 7.12
let rec matrixProduct a b =
    let (D(_, ca)) = dimMatrix a
    let (D(rb, _)) = dimMatrix b
    if ca <> rb 
    then failwith "matrixProduct: matrix product A .* B is defined only if the number of columns of A is the same as the number of rows of B"
    elif not <| corectOrderCheck b C then matrixProduct a (correctOrder b C)
    else
    let (M(vlb, _)) = b
    let product = List.map (
            fun bv -> matrixVectorProduct a bv |> matrixToVector ) vlb
    M(product, C)


type Vector with
    static member (+) (v1, v2) = addVector v1 v2
    static member (-) (v1, v2) = subVector v1 v2
    static member (*) (n, v)  = scalarVector n v
    static member (*) (v, n)  = scalarVector n v
    static member (~-) (v)    = scalarVector (-one) v


type Matrix with
    static member (+) (m1, m2) = addMatrix m1 m2
    static member (+) (m, n)  = dimMatrix m |> matrixOf n |> addMatrix m
    static member (+) (n, m)  = dimMatrix m |> matrixOf n |> addMatrix m
    static member (*) (n, m)  = scalarMatrix m n
    static member (*) (m, n)  = scalarMatrix m n
    static member (*) (m, v)  = matrixVectorProduct m v
    static member (*) (m1, m2) = matrixProduct m1 m2
    static member (/) (m, n)  = scalarMatrix m (inv n)


// Horisontal flip of a matrix
let flip m = 
    let (M(m_new, _)) = correctOrder m C
    List.rev m_new |> matrix


// extracts the first vector of a matrix
let extractfirstVector (M(m, o)) = 
    match m with
    | [] -> failwith "Matrix is empty"
    | x::xs -> x, M(xs, o)

// Last vector of a matrix
let rec extractlastVector m =
    let (v, mf) = flip m |> extractfirstVector 
    (v, flip mf)


// Multiplies two vectors element wise
let vectorMulElementWise (V(u, o1)) (V(v, o2)) =
    if o1 <> o2
    then failwith "Vectors must have the same major order"
    elif List.length u <> List.length v 
    then failwith "Vectors must have the same dimension"
    else
    V (List.map2 (*) u v, o1)

// Conjugates a vector
let conjugateVector (V(v, o)) = 
    V (List.map conjugate v, o)

// Conjugates a matrix
let conjugateMatrix (M(m, o)) = 
    M (List.map conjugateVector m, o)

// Inner product of two vectors
let innerProduct u v =
    let (V(w, _)) = conjugateVector v |> vectorMulElementWise u
    List.fold (+) zero w

// Projection of a vector on another vector    
let proj y x =
    scalarVector (innerProduct x y / innerProduct y y) y

// Dot product of two vectors
let dotProduct (V(u, ou)) (V(v, ov)) = 
    if List.length u <> List.length v 
    then failwith "dotProduct: Vectors most have same length."
    else

    match ou, ov with
    | R, C -> innerProduct (V(u, C)) (V(v, C))
    | _ -> failwith "Missing implementation for this case."


// Orthogonal bacis using the Gram-Schmidt process
let rec orthogonalBacis m =
    if not <| corectOrderCheck m C  
    then orthogonalBacis (correctOrder m C)
    else

    // Gram-Schmidt process but without the normalization
    let rec Gram_Schmidt vm acc_wm =
        match acc_wm [], vm with
        | x, M([], _) -> x 
        | M([], _), M(v1::vrest, o) -> 
            Gram_Schmidt (M(vrest,o)) 
            <| fun x -> extendMatrix (M([v1], C)) x 
        | M(w, _), M(vk::vrest, o) -> 
            let (V(wk, _)) = vk - sumProj w vk
            Gram_Schmidt (M(vrest,o)) 
            <| fun x -> extendMatrix (acc_wm wk) x

    // Sum all the projections of vk on w1 to wk-1
    and sumProj w vk =
        List.map (fun x -> proj x vk) w 
        |> matrix 
        |> sumRows
        |> matrixToVector
        
    Gram_Schmidt m (fun _ -> M([], C))



// Checks if a vector is a zero vector
let isZeroVector (V(v, _)) = 
    List.forall (fun x -> Number.isZero x) v

// Checks if a matrix is a zero matrix
let isZeroMatrix (M(m, _)) = 
    List.forall (fun x -> isZeroVector x) m

// fist non zero element of a vector
let firstNonZero (V(v, _)) =
    if isZeroVector (V(v, C)) then failwith "firstNonZero: Vector does not have a non zero element"
    else
    List.find (fun x -> not (Number.isZero x)) v

// Index of the first non zero element of a vector
let firstNonZeroIndex (V(v, _)) = 
    if isZeroVector (V(v, C)) then -1
    else
    List.findIndex (fun x -> not (Number.isZero x)) v

// string a vector
let stringVector (V(v, o)) =
    let space  = if o = C then "\n" else " "
    List.map (fun x -> toString x) v |> String.concat space

// string a matrix
let rec stringMatrix m =
    if not <| corectOrderCheck m R then stringMatrix (correctOrder m R)
    else
    let (M(vl, _)) = m
    List.map (fun x -> stringVector x) vl |> String.concat "\n"
    
// bool on index
let rec vectorElementIs (V(nl, o)) i b =
    match nl with
    | [] -> false
    | x::_ when i = 0 -> b x
    | _::xs -> vectorElementIs (V(xs, o)) (i - 1) b

// Index of the first non zero element of a matrix
let rec firstNonZeroIndexMatrix (M(m, o)) idx (r, c) = 
    if m = [] then (r, c) else
    let fnxi = List.head m |> firstNonZeroIndex
    match m with
    | [] -> (r, c)
    | _::vt when fnxi >= 0 && (fnxi < c || c = -1) -> firstNonZeroIndexMatrix (M(vt, o)) (idx + 1) (idx, fnxi)
    | _::vt -> firstNonZeroIndexMatrix (M(vt, o)) (idx + 1) (r, c)


// Switches two vectors in a matrix
let swapFirstWith (M(m, o)) i =
    if i = 0 
    then M(m, o)
    else
    let rec swapper h m i idx acc_m =
        match m with
        | [] -> failwith "swapFirstWith: Index out of range"
        | x::xs when idx = i -> x :: acc_m @ (h::xs)
        | x::xs -> swapper h xs i (idx + 1) (acc_m @ [x])

    match m with
    | [] -> failwith "swapFirstWith: Matrix is empty"
    | h::tail -> M(swapper h tail i 1 [], o)

// Muliplies the i'th vector with a scalar 
let scalarIthVector c i (M(m, o)) =
    let rec sIV c m i idx acc_m =
        match m with
        | [] -> failwith "scalarIthVector: Index out of range"
        | x::xs when idx = i -> acc_m @ c * x :: xs
        | x::xs -> sIV c xs i (idx + 1) (acc_m @ [x])
    
    M(sIV c m i 0 [], o)

// Alters row j with Rj <- Rj - c * Ri
let rec rowOperation i j c m = 
    if i = j then failwith "rowOperation: Row i and j must be diffrent j <> i"
    elif not <| corectOrderCheck m R then rowOperation i j c <| correctOrder m R
    else
    replaceMatrixIthVector (j-1) m <| getMatrixIthVector (j-1) m - c * getMatrixIthVector (i-1) m 


// row echelon form of a matrix
let rec rowEchelonForm A = 
    if not <| corectOrderCheck A R then rowEchelonForm (correctOrder A R)
    else
    let (D(r, c)) = dimMatrix A
    match A with
    |M([], _) -> A
    | _ when isZeroMatrix A -> A
    | M(v::_, _) when r = 1 -> firstNonZero v |> inv |> scalarMatrix A  
    | M(_, o) ->
        let (i, j) = firstNonZeroIndexMatrix A 0 (-1 ,-1)
        let (M(B, _)) = swapFirstWith A i
        let b = List.head B |> firstNonZero
        let (M(B, _)) = scalarIthVector (inv b) 0 (M(B, o))
        let R1 = List.head B
        let R2m = List.tail B
        let B = rowOps j 1 r R1 (M(R2m, o))
        let (M(Cm, _)) = rowEchelonForm B
        M(R1::Cm, o)

// Ri <- Ri - b * R1 - posible error i mat 1 notes Algorithm 1 (should be b <- the jth entry og the ith row if B)
and rowOps coloumn i nrows R1 acc_m  =
    if i >= nrows then acc_m else
    let Ri = getMatrixIthVector (i - 1) acc_m
    let b = getVectorIthNumber coloumn Ri
    rowOps coloumn (i + 1) nrows R1 <| replaceMatrixIthVector (i-1) acc_m (Ri - b * R1)

  
// A standard bacis vector of length n with 1 at i
let standardBacisVector n i =
    let rec sbv idx =
        match idx with
        | _ when idx < 1 -> []
        | 1 when i <> 1 -> [zero]
        | x when x = i -> one :: sbv (x - 1)
        | _ -> zero :: sbv (idx - 1)
    vector <| sbv n

// A standard bacis matrix of F^n
let standardBacis n =
    let rec sb idx =
        match idx with
        | _ when idx < 1 -> []
        | 1 -> [standardBacisVector n 1]
        | _ -> standardBacisVector n idx :: sb (idx - 1)
    matrix <| sb n

// fullranked diagonal matrix
let fullrankedDiagonalMatrix m n = 
    if m > n then failwith "fullrankedDiagonalMatrix: number of rows must be less than or equal to the number of columns"
    else
    let rec frdm i frm = 
        match i with
        | _ when i < 0 -> failwith "fullrankedDiagonalMatrix: i must be greater than or equal to 0"
        | 0 -> frm
        | _ ->  
            let (V(zeroV, _)) = vectorOf zero m
            frdm (i - 1) <| extendMatrix frm zeroV
    frdm (n - m) <| standardBacis m


// Checks if a matrix is upper triangular
let rec isUpperTriangular (M(vl, o)) = 
    if not <| corectOrderCheck (M(vl, o)) R then isUpperTriangular (correctOrder (M(vl, o)) R)
    else
    let (D(_, m)) = dimMatrix (M(vl, o))
    let rec iut vl idx =
        match vl with
        | [] -> true
        | x::xs when idx >= m -> isZeroVector x && iut xs (idx + 1)
        | x::xs -> firstNonZeroIndex x = idx && iut xs (idx + 1)
    iut vl 0

// is upper triangular matrix with diagonal elements of 1
let rec isUpperTriangularDiagonal1 (M(vl, o)) = 
    if not <| corectOrderCheck (M(vl, o)) R then isUpperTriangular (correctOrder (M(vl, o)) R)
    else
    let (D(_, m)) = dimMatrix (M(vl, o))
    let rec iut vl idx =
        match vl with
        | [] -> true
        | x::xs when idx >= m -> isZeroVector x && iut xs (idx + 1)
        | x::xs -> 
            let fnz = firstNonZeroIndex x
            vectorElementIs x fnz isOne && 
            fnz = idx && 
            iut xs (idx + 1)
    iut vl 0

// rank of a matrix
let rank m =
    let (D(r, c)) = dimMatrix m
    let maxRank = if r < c then r else c
    let rec ra m rowi =
        if rowi > maxRank || isZeroMatrix m then 
            rowi
        else
            match m with
            | M([], _) -> rowi
            | M(v::vl, o) -> 
                let fnz = firstNonZeroIndex v
                if fnz < rowi then 
                    ra (M(vl, o)) rowi
                else 
                    ra (M(vl, o)) (rowi + 1)
    ra (correctOrder (rowEchelonForm m) R) 0
            
// determines if a matrix has full rank
let hasFullRank m = 
    let (D(r, c)) = dimMatrix m
    let maxRank = if r < c then r else c
    rank m = maxRank

// determines if a list of numbers is all zero
let isZeroNumberList nl =
    List.forall (fun x -> Number.isZero x) nl

// determines if a matrix is Identity matrix
let isDiagonalMatrix m =
    let (D(r, c)) = dimMatrix m
    if r <> c then false
    else
    let rec iim m =
        let nl, (M(vl, o)) = firstElemetsVectors m [] []
        match vl, nl with
        | [], [] -> true
        | v::vTail, n::nTail -> 
            isZeroVector v && isZeroNumberList nTail && not (Number.isZero n) && iim (M(vTail, o))
        | _, _ -> false
    iim m

// checks is if every vector has inner product of zero with the next vector
let rec isOrthogonalBacis m =
    (transposeMatrix m |> conjugateMatrix) * m |> isDiagonalMatrix

// split a matric into two matrices
let rec splitMatrix o i m =
    if not <| corectOrderCheck m o then splitMatrix o i (correctOrder m o)
    else
    let (M(vl, _)) = m

    let rec sm vl idx acc =
        match vl with
        | [] -> failwith "splitMatrix: Index out of range"
        | x::xs when idx = 0 -> matrix <| acc @ [x], matrix xs
        | x::xs -> sm xs (idx - 1) (acc @ [x])
    sm vl i []

// Determines if a matrix has a zero column
let rec dontHaveZeroCols m =
    if not <| corectOrderCheck m C then dontHaveZeroCols (correctOrder m C)
    else
    let (M(vl, _)) = m
    List.forall (fun x -> not <| isZeroVector x) vl


// Determines if a matrix has the same span as another matrix
let hasSameSpanGS mV mW = 
    let (D(r, c)) = dimMatrix mV
    if (D(r, c)) <> dimMatrix mW then failwith "Matrices must have the same dimension"
    else
    
    let Phi m1 m2 = 
        let s1, s2 = 
            extendMatrixWithMatrix m1 m2 
            |> rowEchelonForm 
            |> splitMatrix C (c-1)
        isUpperTriangularDiagonal1 s1 && isUpperTriangularDiagonal1 s2

    Phi mV mW && Phi mW mV
    






/////////////////////////////////
/// Functions to solve Ax = b ///
/////////////////////////////////

type ExprVector = list<Expr<Number>>
type ExprMatrix = list<ExprVector>



// multiplies a expression with a vector and returns a Expr list
let scalarWithExpr (V(nl, _)) e=
        List.map (fun x -> N x * e) nl

// matrix product with expresion vector
let matrixProductExprList vl el =
    let znl = List.head vl |> vectorLength |> vectorOf zero 
    let zeroExprList = scalarWithExpr znl (N zero) // zero expr vector
    List.map2 (fun mc n -> scalarWithExpr n mc) el vl 
    |> List.fold (fun a b -> (List.map2 (+) a b)) zeroExprList

// Creates a vector of n variables
let rec charVector n =
    match n with
    | _ when n <= 0 -> []
    | _ -> X (char (n - 1)) :: charVector (n - 1)

// inserts an eviroment into a vector
let rec vectorEnv n env =
    match n with
    | _ when n <= 0 -> V([], C)
    | _ -> 
        let (V(nl, _)) = vectorEnv (n - 1) env   
        Map.find (char (n - 1)) env :: nl |> vector

// solves the system of lineary equations
let rec solveEquations el bl cl =
        match el, bl, cl with
        | [], [], [] -> Map.empty
        | e::es, b::bs, _ when isZero e && isZero b -> 
            solveEquations es bs cl
        | e::es, b::bs, c::cs ->
                            let env = solveEquations es bs cs
                            let (lhs, rhs) = isolateX (insertEnv e env) b c
                            Map.add (getVariable lhs) (getNumber rhs) env
        | _, _, _ -> failwith "solveEquations: The number of equations and variables must be the same"

// Solves the equation Ax = b
let matrixEquation A (V(nlb, ob)) =
    let (D(r, c)) = dimMatrix A
    if r <> List.length nlb ||  ob = R then 
        failwith "matrixEquation: b must be a column vector or have same length as rows of A"
    
    // Perform row echelon form on the total matrix
    let ef_t = correctOrder (rowEchelonForm <| extendMatrix A nlb) C

    // Extract the last vector of the matrix
    let (V(ef_b, _), M(ef_vl, o)) = extractlastVector ef_t

    // Check if the system of equations has a solution
    if hasFullRank ef_t then 
        failwith "matrixEquation: The system of equations has no solution"
    elif not <| hasFullRank (M(ef_vl, o)) then 
        failwith "matrixEquation: The system of equations has infinite solutions"
    
    let varlist = charVector c
    let b = scalarWithExpr (vector ef_b) (N one)
    solveEquations (matrixProductExprList ef_vl varlist) b varlist |> vectorEnv c 


    


    



    





    
    