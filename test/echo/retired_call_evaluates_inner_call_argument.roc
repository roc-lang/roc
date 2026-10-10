# A call whose earlier argument is a call and whose later argument is undefined is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
f : U64, U64 -> U64
f = |a, b| a + b

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	_ = f(f({
		dbg "deep"
		n
	}, 1), undefined_thing)
	echo!("after\n")
	Ok({})
}
