Lib := [].{
	compute : U64 -> U64
	compute = |n| List.repeat(2.U64, n).fold(0, |a, b| a + b)

	names : List(Str)
	names = ["a", "b"]

	explode : U64 -> U64
	explode = |n| {
		dbg n
		if n > 2 {
			crash "too big"
		} else {
			n
		}
	}
}
