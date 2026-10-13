Tables := [].{
	base : U64
	base = {
		dbg "evaluating base"
		40
	}

	read_by_app_a : U64
	read_by_app_a = base + 2

	read_by_app_b : U64
	read_by_app_b = {
		dbg "evaluating read_by_app_b"
		7
	}

	never_read : U64
	never_read = {
		dbg "evaluating never_read"
		99
	}
}
