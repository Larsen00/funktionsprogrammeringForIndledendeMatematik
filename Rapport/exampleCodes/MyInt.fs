module MyInt
type MyInt = I of int

type MyInt with
    static member (*) (I a, I b) = I (a * b)
    static member (-) (I a, I b) = I (a - b) 

let rec factorial (n:MyInt) =
    match n with
    | I 0 -> I 1
    | I x when x > 0 -> n * factorial (n - I 1)
    | _ -> failwith "Negative argument"