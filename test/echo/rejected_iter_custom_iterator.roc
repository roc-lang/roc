# A `for` loop whose `iter` method returns a custom iterator type is rejected.
# Running anyway evaluates the iterable, then the `iter` call crashes.
Counter := [C(U64)].{
	next : Counter -> [Some(U64, Counter), Done]
	next = |C(k)| if k == 0 { Done } else { Some(k, C(k - 1)) }
}

Bag := [B(U64)].{
	iter : Bag -> Counter
	iter = |B(a)| Counter.C(a)
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	for x in {
		dbg "iterable"
		Bag.B(n)
	} {
		echo!("${x.to_str()}\n")
	}
	Ok({})
}
