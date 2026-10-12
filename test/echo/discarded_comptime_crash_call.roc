# `boom(1)` has constant arguments, so compile-time evaluation runs it, and it
# crashes there. Its value is discarded, but reading that failed value is still
# where the program evaluates it: running the program crashes once it is
# reached, after "before" and never reaching "after".
boom = |_x| {
	crash "boom"
}

main! = |_args| {
	echo!("before")
	boom(1)
	echo!("after")
	Ok({})
}
