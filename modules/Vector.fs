module Vector
open rantionalAndComplex
open Number

type Vector = V of list<Number>
type Matrix = M of list<Vector>

// The dimension of a vector
let dim (V v) = List.length v

// Addision two vectors
let add (V v1) (V v2):Vector =
    if V v1 |> dim  <>  (V v2 |> dim) then failwith "Vectors must have the same dimension"
    else 
    V (List.map2 (+) v1 v2)

// Multiplication of a scalar and a vector
let scalarMul (n:Number) (V v) = V (List.map (fun x -> x * n) v)

let vectorOf n len = V (List.init len (fun _ -> n))

type Vector with
    static member (+) (v1, v2) = add v1 v2
    static member (+) (v, n)  = dim v |> vectorOf n |> add v
    static member (+) (n, v)  = dim v |> vectorOf n |> add v


// let sumRows (M m) = List.fold (+) (zero) m