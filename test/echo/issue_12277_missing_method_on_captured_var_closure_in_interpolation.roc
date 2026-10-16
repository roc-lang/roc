# `List` has no `filter_map`, so the method call is rejected and its
# interpolation with it. The rejected interpolation still evaluates the list
# and the closure `f`, which captures the mutable `$offset`, before it crashes.
main! = |_args| {
	echo!("before")
	var $offset = 10
	f = |x| x + $offset
	echo!("${[1.U32, 2.U32].filter_map(f)}")
	Ok({})
}
