# A method call with an undefined argument is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	x = {
		dbg "receiver"
		n
	}.plus(undefined_thing)
	echo!(x.to_str())
	Ok({})
}
