# A stored capture-bearing closure forces fresh/stored normalization coverage.
Values := [].{
	make : U64 -> (U64 -> U64)
	make = |offset| |value| value + offset

	stored = {
		dbg "lambda solved stored closure"
		make(7)
	}
}

expect Values.stored(3) == 10
