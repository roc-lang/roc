A := { a : Str }

f : A -> Str
f = |value| {
    A.{ b, .. } = value
    b
}

main! = |_| Ok(echo!(f(A.{ a: "" })))
