# A string interpolation with an undefined later part is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	s = "${{
		dbg "first"
		n.to_str()
	}} and ${undefined_thing}"
	echo!(s)
	Ok({})
}
