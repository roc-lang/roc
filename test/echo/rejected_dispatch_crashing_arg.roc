# A call to a rejected `is_eq` evaluates its argument first, so a crash inside
# the argument happens before the dispatch's checked-error crash.
Loose := [L(U64, U64)].{
	is_eq : Loose, Loose -> Bool
	is_eq = |L(a), L(b, _)| a == b
}

main! = |args| {
	n = List.len(args)
	a = Loose.L(n, 2)
	echo!("before\n")
	echo!(Str.inspect(a.is_eq(if n == 0 { crash "argument crashed" } else { a })))
	Ok({})
}
