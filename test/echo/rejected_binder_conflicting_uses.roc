# Each loop below iterates a literal whose default is rejected, so its binder
# binds nothing. Its uses were checked before that rejection, and they
# conflict with each other, directly or through values computed from the
# binder, but they add no report of their own: the literal's rejection is
# the only report for each loop. The independent mistakes beside them are
# still reported. Running the program crashes only once a loop is reached.
conflicting_calls = |_x| {
	for y in 5 {
		a = Str.concat(y, "a")
		b = List.len(y)
		_ = (a, b)
	}
	{}
}

conflicting_elements = |_x| {
	for y in 5 {
		a = [y, "s"]
		b = [y, 1.U8]
		_ = (a, b)
	}
	{}
}

computed_from_binder = |_x| {
	for y in 5 {
		a = Str.concat(y, "a")
		b = List.len(a)
		_ = (a, b)
	}
	{}
}

beside_independent_mistakes = |_x| {
	for y in 5 {
		a = Str.concat(y, "a")
		b = y + 1
		c = Str.concat(1.U8, "x")
		_ = (a, b, c)
	}
	{}
}

main! = |args| {
	echo!("before")
	if List.len(args) > 100 {
		conflicting_elements(1)
		computed_from_binder(1)
		beside_independent_mistakes(1)
	}
	conflicting_calls(1)
	echo!("after")
	Ok({})
}
