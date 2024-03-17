module Matrix
open Number

// row or coloumn major ordere: default of a vector and matric is Column major order.
type order = | R | C

// Rows x Cols
type Dimension  = D of int * int

// a "Normal" is C major and a Transposed one is R major
type Vector = V of list<Number> * order
type Matrix = M of list<Vector> * order

// Construct a vector
let vector nl = V (nl, C)

// Construct a matrix
let matrix vl = M (vl, C)

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
let matrixVectorLength (M(m, _)) = List.head m |> vectorLength

// The dimension of a matrix
let dimMatrix (M(m, o)) =
    let _ = matrixValidMajor (M(m, o))
    let d1 = List.length m
    let d2 = matrixVectorLength (M(m, o))
    match o with
    | R -> D (d1, d2)
    | C -> D (d2, d1)

// Multiplication of a scalar and a vector
let scalarVector (n:Number) (V (v, o)) = 
    V ((List.map (fun x -> x * n) v), o)

// Addision two vectors
let addVector x y =
    let (V (v1, o1)) = x
    let (V (v2, o2)) = y
    if o1 <> o2 then failwith "Vectors must have the same major order"
    elif dimVector x <> dimVector y then failwith "Vectors must have the same dimension"
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

// Alternates the Major order
let alternateOrder o =
    match o with
    | R -> C
    | C -> R

// Alternates the Major order of a matrix
let alternateOrderMatrix (M(m, o)) =
    M (m, alternateOrder o)

// The i'th number of a vector
let getVectorIthNumber (V(v, _)) i = v.[i]

// pops a vector 
let seperateFistNumberFromVector (V(v, o)) = 
    (v.Head, V(v.Tail, o))

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
let giveMatrixHaveSameOrder (M(m1, o1)) (M(m2, o2)) = 
    if o1 <> o2 then changeOrderMatrix (M(m2, o2)) else M(m2, o2)

// Adds two matrices elemtwise
let addMatrix m1 m2 =
    let m3 = giveMatrixHaveSameOrder m1 m2
    if dimMatrix m1 <> dimMatrix m3 then failwith "addMatrix: Matrices must have the same dimension"
    else
    let (M(m1, o)) = m1
    let (M(m3, _)) = m3
    M (List.map2 addVector m1 m3, o)

// Multiplication of a scalar and a matrix
let scalarMatrix (n:Number) (M (m, o)) = 
    M ((List.map (fun x -> scalarVector n x) m), o)

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
    static member (*) (n, v)  = scalarMatrix n v
    static member (*) (v, n)  = scalarMatrix n v


// Changes the order of af matrix to x
let correctOrder (M(m, o)) x =
    if o = x then M(m, o) else changeOrderMatrix (M(m, o))

// Boolean value of if a matrix has the correct order
let corectOrderCheck (M(_, o)) x =
    o = x

// Sum the rows of a matrix
let sumRows m = 
    let (M(m_new, o)) = correctOrder m C
    let v = List.fold (addVector) (matrixVectorLength (M(m_new, o)) |> vectorOf zero) m_new
    matrix [v]

// Horisontal flip of a matrix
let flip m = 
    let (M(m_new, _)) = correctOrder m C
    List.rev m_new |> matrix


// extracts the first vector of a matrix
let extractVector (M(m, o)) = 
    match m with
    | [] -> failwith "Matrix is empty"
    | x::xs -> x, M(xs, o)
    
// Multiplies two vectors element wise
let vectorMulElementWise (V(u, o1)) (V(v, o2)) =
    printfn "%A - %A" u v
    if o1 <> C || o2 <> C then failwith "Vectors must have the same major order"
    elif List.length u <> List.length v then failwith "Vectors must have the same dimension"
    else
    V (List.map2 (*) u v, C)

// Conjugates a vector
let conjugateVector (V(v, o)) = 
    V (List.map conjugate v, o)

// Inner product of two vectors
let innerProduct u v =
    let (V(w, _)) = conjugateVector v |> vectorMulElementWise u
    List.fold (+) zero w

// Dot product of two vectors
let dotProduct (V(u, ou)) (V(v, ov)) = 
    if List.length u <> List.length v 
    then failwith "dotProduct: Vectors most have same length."
    else

    match ou, ov with
    | R, C -> innerProduct (V(u, C)) (V(v, C))
    | _ -> failwith "Missing implementation for this case."

// Projection of a vector on another vector    
let proj y x =
    scalarVector (innerProduct x y / innerProduct y y) y



// Gram-Schmidt process but without the normalization
let rec Gram_Schmidt vm acc_wm =
    if corectOrderCheck vm C |> not then Gram_Schmidt (correctOrder vm C) acc_wm
    else

    match acc_wm [], vm with
    | x, M([], _) -> x 
    | M([], _), M(v1::vrest, o) -> Gram_Schmidt (M(vrest,o)) (fun x -> extendMatrix (M([v1], C)) x) 
    | M(w, _), M(vk::vrest, o) -> 
                let (V(wk, _)) = vk - sumProj w vk
                Gram_Schmidt (M(vrest,o)) (fun x -> extendMatrix (acc_wm wk) x)

// Sum all the projections of vk on w1 to wk-1
and sumProj w vk =
    printfn "sumproj: %A" w
    match w with
    | [] -> vectorOf zero (vectorLength vk)
    | x::xs -> proj x vk + sumProj xs vk
    

// Orthogonal bacis
let orthogonalBacis m =
    Gram_Schmidt m (fun _ -> M([], C))


// Checks if a vector is a zero vector
let isZeroVector (V(v, _)) = 
    List.forall (fun x -> Number.isZero x) v

// Checks if a matrix is a zero matrix
let isZeroMatrix (M(m, _)) = 
    List.forall (fun x -> isZeroVector x) m

// fist non zero element of a vector
let firstNonZero (V(v, _)) = 
    List.find (fun x -> not (Number.isZero x)) v

let rec rowEchelonForm m = 
    if corectOrderCheck m R |> not then rowEchelonForm (correctOrder m R)
    else
    let (D(r, c)) = dimMatrix m

    match m with
    | _ when isZeroMatrix m -> m
    |

    
    