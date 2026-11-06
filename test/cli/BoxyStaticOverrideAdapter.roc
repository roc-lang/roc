BoxyStaticOverrideAdapter :: [].{}

# A closed nominal whose `is_eq` override requires a method at a wider result
# row than the method's own: the override's dictionary for that requirement
# converts the method's result, wherever the descriptor holding the override
# is built.
Ord := [Lo, Hi].{
	cmp : Ord, Ord -> [Less, Same, More]
	cmp = |a, b|
		match (a, b) {
			(Lo, Hi) => Less
			(Hi, Lo) => More
			_ => Same
		}
}

Pair(a) := { v : a }.{
	is_eq : Pair(a), Pair(a) -> Bool where [a.cmp : a, a -> [Abort, Less, Same, More]]
	is_eq = |x, y|
		match x.v.cmp(y.v) {
			Same => Bool.True
			_ => Bool.False
		}
}

same : t, t -> Bool where [t.is_eq : t, t -> Bool]
same = |x, y| x == y

lo : Pair(Ord)
lo = { v: Ord.Lo }

hi : Pair(Ord)
hi = { v: Ord.Hi }

expect same([lo], [lo])
expect !same([lo], [hi])
expect lo == lo
