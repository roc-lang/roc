# https://github.com/roc-lang/roc/issues/12897
# A destructure that cannot match, in an unannotated top-level constant, is
# reported as a non-exhaustive destructure.
x = {
	Ok(y) = Err({})
	y
}

main! = |_args| {
	echo!(Str.inspect(x))
	Ok({})
}
