# Structural equality with an undefined right operand is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	b = {
		dbg "lhs"
		n
	} == undefined_thing
	echo!(Str.inspect(b))
	Ok({})
}
