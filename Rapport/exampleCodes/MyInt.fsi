module MyInt

type MyInt = I of int
    with
    static member (*) : MyInt * MyInt -> MyInt
    static member (-) : MyInt * MyInt -> MyInt

val factorial : MyInt -> MyInt