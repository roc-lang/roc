Lib := [].{
	table : List(U64)
	table = {
		dbg "evaluating table"
		[1, 2, 3]
	}

	sum_table : U64 -> U64
	sum_table = |n| n + List.sum(table)

	double : U64 -> U64
	double = |n| n * 2
}
