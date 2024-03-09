module Complex

[<Sealed>]
type complex<'a> =
    static member ( * ) : complex<'a> * complex<'a> -> complex<'a>
    static member ( * ) : complex<'a> * 'a -> complex<'a>
    static member ( * ) : 'a * complex<'a> -> complex<'a>

