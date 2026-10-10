# `==` on a type whose `is_eq` is rejected evaluates both operands, including
# the `dbg` inside each, before it crashes as a checked error.
Loose := [L(U64, U64)].{
	is_eq : Loose, Loose -> Bool
	is_eq = |L(a), L(b, _)| a == b
}

main! = |args| {
	n = List.len(args)
	a = Loose.L(n, 2)
	b = Loose.L(n, 3)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "lhs"
		a
	} == {
		dbg "rhs"
		b
	}))
	Ok({})
}
