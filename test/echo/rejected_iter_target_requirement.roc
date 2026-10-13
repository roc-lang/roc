# A `for` loop whose `iter` method has a where clause the item type does not
# satisfy. The loop's `iter` call selects `Bag.iter`, whose `shout`
# requirement checking rejected, so the call crashes as a checked error once
# the iterable is evaluated, in every lowering mode.
Bag(a) := [B(a)].{
	iter : Bag(a) -> Iter(a) where [a.shout : a -> a]
	iter = |B(x)| [x.shout()].iter()
}

main! = |_| {
	echo!("before\n")
	for x in Bag.B("s") {
		echo!(x)
	}
	Ok({})
}
