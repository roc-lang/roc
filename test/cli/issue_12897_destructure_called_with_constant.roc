# https://github.com/roc-lang/roc/issues/12897
# A destructure that cannot match, in a function called with a constant
# argument, is reported as a non-exhaustive destructure.
g = |_| {
	Ok(y) = Err({})
	y
}

main! = |_args| {
	echo!(Str.inspect(g({})))
	Ok({})
}
