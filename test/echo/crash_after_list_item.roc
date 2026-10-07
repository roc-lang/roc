# A list whose later element always crashes evaluates its earlier element,
# including the `dbg` inside it, before that crash.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	l : List(U64)
	l = [{
		dbg "first"
		n
	}, crash "second"]
	echo!(Str.inspect(l))
	Ok({})
}
