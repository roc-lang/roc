# A generic where-clause dispatch whose evidence is a rejected `is_eq`
# evaluates its operands, including the `dbg` inside one, before it crashes as
# a checked error.
Loose := [L(U64, U64)].{
	is_eq : Loose, Loose -> Bool
	is_eq = |L(a), L(b, _)| a == b
}

eq_generic : a, a -> Bool where [a.is_eq : a, a -> Bool]
eq_generic = |x, y| x.is_eq({
	dbg "operand"
	y
})

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect(eq_generic(Loose.L(n, 2), Loose.L(n, 3))))
	Ok({})
}
