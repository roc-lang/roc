# A for loop over a call with an undefined argument is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
f : U64, U64 -> List(U64)
f = |a, b| [a, b]

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	for x in f({
		dbg "first"
		n
	}, undefined_thing) {
		echo!(Str.inspect(x))
	}
	Ok({})
}
