#r "nuget: FsCheck"
open FsCheck

// proposition formula: bool -> bool -> bool -> bool
let propositional_formula P Q Y =
    (P && ( Q && Y)) = ((P && Q) && Y)

let propositional_formula_invalid P Q Y =
    P && ( Q && Y) = (P && Q) && Y




