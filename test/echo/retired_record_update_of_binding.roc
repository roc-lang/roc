# A record update of a binding with an undefined field is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	base = { a: n, b: n }
	r = { ..base, b: undefined_thing }
	echo!(Str.inspect(r))
	Ok({})
}
