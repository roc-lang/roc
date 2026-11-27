Loop :: [].{
	ping : a, U64, Str -> Str
	ping = |x, n, acc| if n == 0 acc else Loop.pong(x, n - 1, Str.concat(acc, "i"))

	pong : a, U64, Str -> Str
	pong = |x, n, acc| if n == 0 acc else Loop.ping(x, n - 1, Str.concat(acc, "o"))
}
