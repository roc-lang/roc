# A direct method call to a rejected `is_eq` evaluates its receiver and its
# argument, including the `dbg` inside each, before it crashes as a checked
# error.
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
		dbg "receiver"
		a
	}.is_eq({
		dbg "x"
		b
	})))
	Ok({})
}
