# A compile-time constant whose evaluation reaches a rejected `is_eq` stores a
# checked error; reading it at runtime crashes as that checked error.
Loose := [L(U64, U64)].{
	is_eq : Loose, Loose -> Bool
	is_eq = |L(a), L(b, _)| a == b
}

mk : U64, U64 -> Loose
mk = |a, b| L(a, b)

same = mk(1, 2) == mk(1, 3)

main! = |_args| {
	echo!("before\n")
	echo!("${Str.inspect(same)}\n")
	Ok({})
}
