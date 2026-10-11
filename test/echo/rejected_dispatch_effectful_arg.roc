# A call to a rejected `is_eq` performs the effects of its argument before it
# crashes as a checked error.
Loose := [L(U64, U64)].{
	is_eq : Loose, Loose -> Bool
	is_eq = |L(a), L(b, _)| a == b
}

make! : U64 => Loose
make! = |n| {
	echo!("effect\n")
	Loose.L(n, 3)
}

main! = |args| {
	n = List.len(args)
	a = Loose.L(n, 2)
	echo!("before\n")
	echo!(Str.inspect(a.is_eq(make!(n))))
	Ok({})
}
