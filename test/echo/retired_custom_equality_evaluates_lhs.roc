# Custom equality with an undefined right operand is rejected. Running anyway evaluates, in order, the
# operands before the undefined one, with their `dbg`, then crashes where the
# undefined operand is evaluated.
Thing := [T(U64)].{
	is_eq : Thing, Thing -> Bool
	is_eq = |T(a), T(b)| a == b
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	b = {
		dbg "lhs"
		Thing.T(n)
	} == undefined_thing
	echo!(Str.inspect(b))
	Ok({})
}
