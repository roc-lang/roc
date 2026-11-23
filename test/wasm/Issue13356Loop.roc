# Two functions with a generic parameter that call each other in tail
# position, so each takes a tail carry.
Issue13356Loop :: [].{
	ping : a, U64, Str -> Str
	ping = |x, n, acc| if n == 0 acc else Issue13356Loop.pong(x, n - 1, Str.concat(acc, "i"))

	pong : a, U64, Str -> Str
	pong = |x, n, acc| if n == 0 acc else Issue13356Loop.ping(x, n - 1, Str.concat(acc, "o"))
}
