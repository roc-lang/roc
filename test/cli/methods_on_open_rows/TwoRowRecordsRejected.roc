# Both records ending in the row variable must have the method: a call that
# supplies a named type without it for one of them is rejected.

TwoRowRecordsRejected :: [].{}

Num := { a : U64, b : U64 }.{
	is_eq : Num, Num -> Bool
	is_eq = |_, _| Bool.True
}

Bare := { a : Str, b : U64 }

both : { a : U64, ..r }, { a : Str, ..r } -> (Bool, Bool) where [r.is_eq : r, r -> Bool]
both = |x, y| (x == x, y == y)

expect both(Num.{ a: 1, b: 2 }, Bare.{ a: "s", b: 2 }) == (Bool.True, Bool.True)
