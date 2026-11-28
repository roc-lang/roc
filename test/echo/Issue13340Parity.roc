# The first pair forwards its parameter and has no carry. The wide pair
# constructs a record, so the imported entry still needs the carry ABI.
Issue13340Parity :: [].{
	is_even : a, U64 -> Bool
	is_even = |x, n| if n == 0 Bool.True else Issue13340Parity.is_odd(x, n - 1)

	is_odd : a, U64 -> Bool
	is_odd = |x, n| if n == 0 Bool.False else Issue13340Parity.is_even(x, n - 1)

	wide_even : a, U64, U64, U64, U64, U64, U64, U64, U64, U64 -> U64
	wide_even = |x, n, b, c, d, e, f, g, h, i|
		if n == 0 b + c + d + e + f + g + h + i else Issue13340Parity.wide_odd({ x, n }, n - 1, b, c, d, e, f, g, h, i)

        wide_odd : { x : a, n : U64 }, U64, U64, U64, U64, U64, U64, U64, U64, U64 -> U64
        wide_odd = |state, n, b, c, d, e, f, g, h, i|
		if n == 0 0 else Issue13340Parity.wide_even(state.x, n - 1, b, c, d, e, f, g, h, i)
}
