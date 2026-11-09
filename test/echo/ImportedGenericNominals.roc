ImportedGenericNominals :: [].{
	Pair(w) := { a : (w, w) }

	pair : w, w -> Pair(w)
	pair = |x, y| { a: (x, y) }

	pair_u64 : U64, U64 -> Pair(U64)
	pair_u64 = |x, y| { a: (x, y) }

	Nested(w) := { a : { b : w, c : w } }

	nested : w, w -> Nested(w)
	nested = |x, y| { a: { b: x, c: y } }

	Wrapped(w) := [Two((w, w)), Empty]

	wrapped : w, w -> Wrapped(w)
	wrapped = |x, y| Two((x, y))
}
