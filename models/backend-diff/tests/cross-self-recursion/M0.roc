M0 :: [].{
	nt : U64, Str -> (I64 -> I64)
	nt = |n, acc| if n == 0 (|p| p + 2) else {
		r = M0.nt(n - 1, if Bool.False acc else "x")
		_v = P(5)
		r
	}
}
