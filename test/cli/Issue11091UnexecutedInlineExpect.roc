check = |run| {
	if run {
		expect 1 == 2
	} else {}
	Bool.True
}

expect check(Bool.False)
