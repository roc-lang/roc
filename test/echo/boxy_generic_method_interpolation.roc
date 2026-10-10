## An unannotated method whose result is an interpolated string is generic in
## that result, so its literal's assembler is adapted to each use's type when
## it is frozen at compile time.

Counter := { count : U64 }.{
	describe = |c| "a${c.count.to_str()}b"
}

main! = |_args| {
	counter = Counter.{ count: 1 }
	echo!(counter.describe())
	Ok({})
}
