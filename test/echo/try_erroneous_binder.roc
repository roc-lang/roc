# `undefined_fn` is not in scope, so the value `?` unwraps is erroneous and
# `y` binds nothing. The method call on `y` adds no report of its own, and
# running the program crashes only once `run` is reached.
run = |x| {
	y = undefined_fn(x)?
	Ok(y.to_str())
}

main! = |_args| {
	echo!("before")
	_ = run(1)
	echo!("after")
	Ok({})
}
