# Two generic functions that call each other in tail position, so each takes
# the hidden parameters of a procedure a frame-replacing call can enter.
Issue13340Parity :: [].{
	is_even : a, U64 -> Bool
	is_even = |x, n| if n == 0 Bool.True else Issue13340Parity.is_odd(x, n - 1)

	is_odd : a, U64 -> Bool
	is_odd = |x, n| if n == 0 Bool.False else Issue13340Parity.is_even(x, n - 1)

	wide_even : a, U64, U64, U64, U64, U64, U64, U64, U64, U64 -> U64
	wide_even = |x, n, b, c, d, e, f, g, h, i|
		if n == 0 b + c + d + e + f + g + h + i else Issue13340Parity.wide_odd(x, n - 1, b, c, d, e, f, g, h, i)

	wide_odd : a, U64, U64, U64, U64, U64, U64, U64, U64, U64 -> U64
	wide_odd = |x, n, b, c, d, e, f, g, h, i|
		if n == 0 0 else Issue13340Parity.wide_even(x, n - 1, b, c, d, e, f, g, h, i)
}
