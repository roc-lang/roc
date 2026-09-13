A := { a : Str }

f : A -> Str
f = |value| match value {
    A.{} => ""
}

main! = |_| Ok(echo!(f(A.{ a: "" })))
