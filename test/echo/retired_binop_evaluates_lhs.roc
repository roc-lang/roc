# An arithmetic operator with an undefined right operand is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	x = {
		dbg "lhs"
		n
	} + undefined_thing
	echo!(x.to_str())
	Ok({})
}
