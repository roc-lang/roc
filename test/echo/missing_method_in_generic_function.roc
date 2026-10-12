# A generic function that calls a missing method on a record evaluates its
# statements and the call's operands when called, then crashes.
wrap = |x| {
	dbg "wrap"
	{
		dbg "receiver"
		{ v: x }
	}.missing({
		dbg "arg"
		1
	})
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect(wrap(n)))
	echo!(Str.inspect(wrap("s")))
	Ok({})
}
