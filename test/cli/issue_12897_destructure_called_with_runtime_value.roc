# https://github.com/roc-lang/roc/issues/12897
# A destructure that cannot match, in a function only called with a runtime
# value, is reported as a non-exhaustive destructure.
g = |_| {
	Ok(y) = Err({})
	y
}

main! = |args| {
	echo!(Str.inspect(g(args)))
	Ok({})
}
