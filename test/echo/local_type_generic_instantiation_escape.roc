# An unannotated function whose body declares a type with a method that
# captures `offset`, generalized over the collection it maps. Methods never
# capture, so the method is rejected at `offset`, and calling it crashes.

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
