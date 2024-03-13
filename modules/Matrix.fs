module Matrix
open Number

// row or coloumn major ordere: default of a vector and matric is Column major order.
type order = | R | C
// Rows x Cols
type Dimension  = D of int * int
// a "Normal" is C major and a Transposed one is R major
type Vector = V of list<Number> * order
type Matrix = M of list<Vector> * order

let vector nl = V (nl, C)
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

let vectorLength (V(v, _)) = List.length v
let matrixVectorLength (M(m, _)) = List.head m |> vectorLength

// The dimension of a matrix
let dimMatrix (M(m, o)) =
    let _ = matrixValidMajor (M(m, o))
    let d1 = List.length m
    let d2 = matrixVectorLength (M(m, o))
    match o with
    | R -> D (d1, d2)
    | C -> D (d2, d1)


// Addision two vectors
let addVector x y =
    let (V (v1, o1)) = x
    let (V (v2, o2)) = y
    if o1 <> o2 then failwith "Vectors must have the same major order"
    elif dimVector x <> dimVector y then failwith "Vectors must have the same dimension"
    else 
    V ((List.map2 (+) v1 v2), o1)

// Multiplication of a scalar and a vector
let scalarVector (n:Number) (V (v, o)) = 
    V ((List.map (fun x -> x * n) v), o)

// Construct a vector of n with length len
let vectorOf n len = V ((List.init len (fun _ -> n)), C)

let matrixOf n (D (r, c)) = M ((List.init c (fun _ -> vectorOf n r)), C)

// Transposes a vector
let transposeVector (V(v, o)) =
    match o with
    | R -> V(v, C)
    | C -> V(v, R)

let extendVector (V(v, o)) n =
    V(v @ [n], o)

// Extend a matrix with a number list
let extendMatrix (M(m, o)) nl =
    if m <> [] && List.length nl <> matrixVectorLength (M(m, o)) then failwith "The list must have the same length as the matrix's vectors"
    else
    M (m @ [(V (nl, o))], o)

// Alternates the Major order
let alternateOrder o =
    match o with
    | R -> C
    | C -> R

let alternateOrderMatrix (M(m, o)) =
    M (m, alternateOrder o)

// The i'th number of a vector
let getVectorIthNumber (V(v, _)) i = v.[i]

let seperateFistNumberFromVector (V(v, o)) = (v.Head, V(v.Tail, o)) // LAVER FEJL

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


let changeOrderMatrix (M(m, o)) =
    alternateOrderMatrix (M([], o)) |> chaingingOrderMatrix (M(m, o)) 

let giveMatrixHaveSameOrder (M(m1, o1)) (M(m2, o2)) = 
    if o1 <> o2 then changeOrderMatrix (M(m2, o2)) else M(m2, o2)


let addMatrix m1 m2 =
    let m3 = giveMatrixHaveSameOrder m1 m2
    if dimMatrix m1 <> dimMatrix m3 then failwith "addMatrix: Matrices must have the same dimension"
    else
    let (M(m1, o)) = m1
    let (M(m3, _)) = m3
    M (List.map2 addVector m1 m3, o)

let scalarMatrix (n:Number) (M (m, o)) = 
    M ((List.map (fun x -> scalarVector n x) m), o)


type Matrix with
    static member (+) (m1, m2) = addMatrix m1 m2
    static member (+) (m, n)  = dimMatrix m |> matrixOf n |> addMatrix m
    static member (+) (n, m)  = dimMatrix m |> matrixOf n |> addMatrix m
    static member (*) (n, v)  = scalarMatrix n v
    static member (*) (v, n)  = scalarMatrix n v


// Changes the order of af matrix to x
let correctOrder (M(m, o)) x =
    if o = x then M(m, o) else changeOrderMatrix (M(m, o))

// Sum the rows of a matrix
let sumRows m = 
    let (M(m_new, o)) = correctOrder m C
    let v = List.fold (addVector) (matrixVectorLength (M(m_new, o)) |> vectorOf zero) m_new
    matrix [v]

// Horisontal flip of a matrix
let flip m = 
    let (M(m_new, _)) = correctOrder m C
    List.rev m_new |> matrix



