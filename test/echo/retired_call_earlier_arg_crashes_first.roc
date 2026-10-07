# A call whose earlier argument crashes before its undefined argument is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
f : U64, U64 -> U64
f = |a, b| a + b

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	_ = f({
		dbg "first"
		n
	}, {
		crash "second"
	}.plus(undefined_thing))
	Ok({})
}
