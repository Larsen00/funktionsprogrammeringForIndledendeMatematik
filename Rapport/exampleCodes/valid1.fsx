#r "nuget: FsCheck"
open FsCheck

let propositional_formula P Q R =
    (P && ( Q && R)) = ((P && Q) && R)

let _ = Check.Quick propositional_formula



