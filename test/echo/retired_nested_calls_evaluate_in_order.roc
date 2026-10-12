# Nested calls whose innermost argument is undefined is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
f : U64, U64 -> U64
f = |a, b| a + b

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	_ = f({
		dbg "outer"
		n
	}, f({
		dbg "inner"
		n
	}, undefined_thing))
	Ok({})
}
