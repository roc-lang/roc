main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!("${Str.inspect((mk(n, 2), 1.U64) == (mk(1, 3), 1.U64))}\n")
	Ok({})
}

Loose := [L(U64, U64)].{
	is_eq : Loose, Loose -> Bool
	is_eq = |L(a), L(b, _)| a == b
}

mk : U64, U64 -> Loose
mk = |a, b| L(a, b)
