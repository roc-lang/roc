# A record with an undefined later field is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	r = { b: {
		dbg "first"
		n
	}, a: undefined_thing }
	echo!(Str.inspect(r))
	Ok({})
}
