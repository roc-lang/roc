## A generic comparison of an open tag row that reaches a type whose `is_eq`
## declaration checking rejected crashes as a checked error, whether the
## comparison is specialized or guided by runtime descriptors.

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!("${Str.inspect(same(Pair(mk(n, 2), 1.U64), Pair(mk(1, 3), 1.U64)))}\n")
	Ok({})
}

Loose := [L(U64, U64)].{
	is_eq : Loose, Loose -> Bool
	is_eq = |L(a), L(b, _)| a == b
}

mk : U64, U64 -> Loose
mk = |a, b| L(a, b)

same = |a, b| if a == Nope { False } else { a == b }
