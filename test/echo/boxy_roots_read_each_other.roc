# The hoisted constants `pong(1)` and `ping(1)` each read the other, so
# evaluating either in place would recurse forever: both fail with the stack
# overflow that recursion ends in, under either strategy.
ping : U64 -> U64
ping = |n| {
	x = pong(1)
	x + n
}

pong : U64 -> U64
pong = |n| {
	y = ping(1)
	y + n
}

main! = |args| {
	n = List.len(args) + 1
	echo!(Str.inspect(ping(n)))
	Ok({})
}
