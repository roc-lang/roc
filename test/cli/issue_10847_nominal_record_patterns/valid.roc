A := { a : Str }.{
    f : A -> Str
    f = |A.{ .. }| ""
}

main! = |_| Ok(echo!(A.f(A.{ a: "" })))
