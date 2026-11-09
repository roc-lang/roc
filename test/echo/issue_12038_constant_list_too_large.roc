# A top-level constant is evaluated at compile time, and a list of
# `U64.highest` bytes cannot be allocated, so its evaluation crashes there.
buffer = List.repeat(0.U8, U64.highest)

main! = |_args| {
	echo!(buffer.len().to_str())
	Ok({})
}
