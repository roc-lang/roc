# Structural equality whose component reaches a rejected `is_eq` evaluates
# both operands, including the `dbg` inside each, before it crashes as a
# checked error.
Loose := [L(U64, U64)].{
	is_eq : Loose, Loose -> Bool
	is_eq = |L(a), L(b, _)| a == b
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "lhs"
		(Loose.L(n, 2), 1.U64)
	} == {
		dbg "rhs"
		(Loose.L(1, 3), 1.U64)
	}))
	Ok({})
}
