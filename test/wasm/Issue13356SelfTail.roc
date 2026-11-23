# A function whose recursive call ends up in tail position once the unused
# binding after it is dropped, so it is a one-member cycle of tail calls.
Issue13356SelfTail :: [].{
	nt : U64, Str -> (I64 -> I64)
	nt = |n, acc| if n == 0 (|p| p + 2) else {
		r = Issue13356SelfTail.nt(n - 1, if n == 99 acc else "x")
		_v = P(5)
		r
	}
}
