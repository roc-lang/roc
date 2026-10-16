# `a` refers to itself through the closure it passes to `lazy`, so it is a
# recursive value. `lazy` ignores that closure and returns one that captures
# nothing, so `a`'s value is zero-sized.
lazy : (U8 -> U8) -> (U8 -> U8)
lazy = |_f| |x| x + 1

a = lazy(|y| a(y))

main! = |_args| {
	echo!(a(41).to_str())
	Ok({})
}
