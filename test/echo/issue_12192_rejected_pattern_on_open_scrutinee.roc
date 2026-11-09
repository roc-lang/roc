# `Demo.A` names no tag (the empty `Demo` only declares `Choice`), so the
# record in `make` and the pattern in the `match` are both rejected. `make`'s
# body crashes, so nothing fixes its result type, and the rejected pattern does
# not write its error into the open type of `x.x`: `x = make()` still lowers,
# and the program crashes inside `make`.
Demo :: [].{
	Choice := [A]
}

make = || { x: Demo.A }

main! = |_args| {
	echo!("before")
	x = make()
	echo!(Str.inspect(match x.x {
		Demo.A => Bool.True
	}))
	Ok({})
}
