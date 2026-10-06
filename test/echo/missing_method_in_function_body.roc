# A function whose body calls a missing method evaluates its statements and the
# call's operands when called, then crashes.
h : U64 -> U64
h = |x| {
	dbg "body"
	x.not_a_method({
		dbg "arg"
		1
	})
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect(h(n)))
	Ok({})
}
