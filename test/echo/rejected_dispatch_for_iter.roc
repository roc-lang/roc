# A `for` loop whose `iter` method is rejected evaluates its iterable,
# including the `dbg` inside it, before the `iter` call crashes as a checked
# error.
Bag := [B(U64, U64)].{
	iter : Bag -> Iter(U64)
	iter = |B(a)| [a].iter()
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	for x in {
		dbg "iterable"
		Bag.B(n, 1)
	} {
		echo!("${x.to_str()}\n")
	}
	Ok({})
}
