# One row variable can end two record types that differ in a field: an
# annotation states the requirement for each, and each call answers it with
# the type it supplies for each one.

TwoRowRecords :: [].{}

Num := { a : U64, b : U64 }.{
	is_eq : Num, Num -> Bool
	is_eq = |_, _| Bool.True
}

Text := { a : Str, b : U64 }.{
	is_eq : Text, Text -> Bool
	is_eq = |_, _| Bool.False
}


both : { a : U64, ..r }, { a : Str, ..r } -> (Bool, Bool) where [r.is_eq : r, r -> Bool]
both = |x, y| (x == x, y == y)

expect both(Num.{ a: 1, b: 2 }, Text.{ a: "s", b: 2 }) == (Bool.True, Bool.False)
expect both({ a: 1, b: 2 }, { a: "s", b: 2 }) == (Bool.True, Bool.True)
