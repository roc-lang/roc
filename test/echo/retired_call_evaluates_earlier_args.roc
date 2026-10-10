# A call with an undefined argument is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
f : U64, U64 -> U64
f = |a, b| a + b

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect(f({
		dbg "first"
		n
	}, undefined_thing)))
	Ok({})
}
