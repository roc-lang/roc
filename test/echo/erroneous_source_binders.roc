# `undefined_fn` is not in scope, so every value matched from its result is
# erroneous. Each name a `match` branch, a destructure, or a `for` loop binds
# from it binds nothing, so the method calls on those names add no report of
# their own, and running the program crashes only once one of them is reached.
branch = |x|
	match undefined_fn(x) {
		Ok(y) => y.to_str()
		Err(_) => ""
	}

destructure = |x| {
	(a, _b) = undefined_fn(x)
	a.to_str()
}

loop = |x| {
	var $acc = ""
	for y in undefined_fn(x) {
		$acc = y.to_str()
	}
	$acc
}

main! = |args| {
	echo!("before")
	if List.len(args) > 100 {
		echo!(branch(1))
		echo!(loop(1))
	}
	echo!(destructure(1))
	echo!("after")
	Ok({})
}
