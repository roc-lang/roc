Parity :: [].{
	even : U64, U64 -> U64
	even = |n, acc| if n == 0 acc else Parity.odd(n - 1, acc + 1)

	odd : U64, U64 -> U64
	odd = |n, acc| if n == 0 acc else Parity.even(n - 1, acc + 1)
}
