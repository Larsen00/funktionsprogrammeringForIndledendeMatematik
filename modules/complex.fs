module complex
type complex<'a> = | Cn of 'a * 'a

// multiplication of Complex number with scalar
let mulS (Cn (a, b)) s =  Cn (a * s, b * s)

type complex<'a> with
    static member (*)  (Cn (a, b), Cn (c, d)) = Cn (a*c-b*d, b*c+a*d) 
    static member (*)  (c, a)          = mulS c a
    static member (*)  (a, c)          = mulS c a



