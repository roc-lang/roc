# A type method call with an undefined argument is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
make : U64, U64 -> a where [a.combine : U64, U64 -> a]
make = |x, y| {
	A : a
	A.combine({
		dbg "first"
		x
	}, undefined_thing)
}

Thing := [T(U64)].{
	combine : U64, U64 -> Thing
	combine = |a, b| T(a + b)
}

main! = |args| {
	echo!("before\n")
	t : Thing
	t = make(List.len(args), 1)
	echo!(Str.inspect(t))
	Ok({})
}
