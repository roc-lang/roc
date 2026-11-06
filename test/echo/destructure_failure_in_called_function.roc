# The block in `g` is a hoisted root whose type is not context-free, so it
# never evaluates on its own: the root reading `g({})` evaluates the block in
# place, and the destructure's failure is reported where that root reaches it.
g = |_| {
	Ok(y) = Err({})
	y
}

main! = |_args| {
	echo!(Str.inspect(g({})))
	Ok({})
}
