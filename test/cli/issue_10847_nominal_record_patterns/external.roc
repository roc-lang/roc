import Record

f : Record -> Str
f = |Record.Bad(b)| b

main! = |_| Ok(echo!(f(Record.{ a: "" })))
