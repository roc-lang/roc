A := { a : Str }

f : (A, Str) -> Str
f = |(A.{ b, .. }, _)| b

main! = |_| Ok(echo!(f((A.{ a: "" }, ""))))
