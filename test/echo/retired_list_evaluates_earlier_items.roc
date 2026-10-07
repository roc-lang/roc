# A list with an undefined later item is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	l = [{
		dbg "first"
		n
	}, undefined_thing]
	echo!(Str.inspect(l))
	Ok({})
}
