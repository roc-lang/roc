Helper :: {}.{
	check : I64 -> Bool
	check = |x| {
		expect x == 2
		Bool.True
	}
}

expect Helper.check(1)
