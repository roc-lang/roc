# A call to a method a nominal type does not have evaluates its receiver and
# its arguments, then crashes.
Thing := [T(U64)].{
	get : Thing -> U64
	get = |T(x)| x
}

main! = |args| {
	t = Thing.T(List.len(args))
	echo!("before\n")
	echo!(Str.inspect({
		dbg "receiver"
		t
	}.missing({
		dbg "arg"
		1
	})))
	Ok({})
}
