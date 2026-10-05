# An unannotated function whose body declares a type with a capturing method
# is generalized over the collection it maps, so its caller instantiates it
# at that type: the type leaves its block through the instantiation, which is
# reported where the caller uses the function.

total_of = |base, counts| {
	offset = base * 10
	Counter := { count : U64 }.{
		value = |c| c.count + offset
	}

	counters = counts.map(|n| Counter.{ count: n })
	values = counters.map(|c| c.value())
	List.sum(values)
}

main! = |_args| {
	echo!("${total_of(1, [1, 2, 3]).to_str()}\n")
	Ok({})
}
