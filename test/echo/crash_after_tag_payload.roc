# A tag whose later payload always crashes evaluates its earlier payload,
# including the `dbg` inside it, before that crash.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	t : [Pair(U64, U64)]
	t = Pair({
		dbg "tag"
		n
	}, crash "second")
	echo!(Str.inspect(t))
	Ok({})
}
