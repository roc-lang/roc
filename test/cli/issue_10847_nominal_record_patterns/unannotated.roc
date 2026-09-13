A := { a : Str }

f = |A.{}| ""

main! = |_| Ok(echo!(f(A.{ a: "" })))
