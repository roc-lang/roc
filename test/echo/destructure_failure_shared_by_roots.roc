# Two hoisted roots call into the same block that only compile-time code
# reaches. Both fail at its destructure; it is reported once.
g = |_| {
	Ok(y) = Err({})
	y
}

main! = |_args| {
	echo!(Str.inspect(g({})))
	echo!(Str.concat("a", Str.inspect(g({}))))
	Ok({})
}
