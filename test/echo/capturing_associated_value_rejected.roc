# An associated value of a type declared in a function body may not capture a
# value of that body. `roc check` reports it at the capture. The program still
# runs: declaring the rejected value does nothing, other associated values
# work, and a lookup of the rejected value crashes.

make! = |offset| {
	Counter := { count : U64 }.{
		start = Counter.{ count: offset }
		zero = Counter.{ count: 0 }
	}
	echo!("zero ${Counter.zero.count.to_str()}\n")
	Counter.start.count
}

main! = |args| {
	echo!("${make!(List.len(args) + 4).to_str()}\n")
	Ok({})
}
