# A top-level constant holding a boxed function is immutable data. Every read
# of it, including one that stores it in a record field, sees the same
# callable.
make_probe : () -> Box(U64 -> U64)
make_probe = || {
	probe : U64 -> U64
	probe = |value| value + 1

	Box.box(probe)
}

shared_probe : Box(U64 -> U64)
shared_probe = make_probe()

pair : () -> { first : Box(U64 -> U64), second : Box(U64 -> U64) }
pair = || { first: shared_probe, second: shared_probe }

main! = |_args| {
	fields = pair()
	first = Box.unbox(fields.first)
	second = Box.unbox(fields.second)
	direct = Box.unbox(shared_probe)
	echo!(Str.inspect((first(41), second(1), direct(9))))
	Ok({})
}
