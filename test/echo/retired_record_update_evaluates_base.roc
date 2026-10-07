# A record update with an undefined field is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
f : U64, U64 -> U64
f = |a, b| a + b

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	r = { ..{ a: {
		dbg "base"
		n
	}, b: n }, b: undefined_thing }
	echo!(Str.inspect(r))
	Ok({})
}
