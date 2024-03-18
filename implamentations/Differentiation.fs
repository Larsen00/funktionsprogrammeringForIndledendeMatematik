module Differentiation

open Expression
open Number

let rec diff f dx = 
    match f with
    | X a when a = dx -> N (Int 1)
    | X _ -> N (Int 0)
    | N a -> N (Int 0)
    | Neg a -> diff (Mul (N (Int 1), a)) dx
    | Add(a, b) -> Add(diff a dx, diff b dx)
    | Sub(a, b) -> Sub(diff a dx, diff b dx)
    | Mul(a, b) -> Add(Mul(diff a dx, b), Mul(a, diff b dx))
    | Div(a, b) -> Div(Sub(Mul(diff a dx, b), Mul(a, diff b dx)), Mul(b, b))
