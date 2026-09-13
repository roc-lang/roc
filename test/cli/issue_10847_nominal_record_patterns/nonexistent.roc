A := { a : Str }.{
    f : A -> Str
    f = |A.{ b, .. }| b
}

main! = |_| Ok(echo!(A.f(A.{ a: "" })))
