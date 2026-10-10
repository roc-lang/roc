# A tuple whose later item always crashes evaluates its earlier item,
# including the `dbg` inside it, before that crash.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	t : (U64, U64)
	t = ({
		dbg "first"
		n
	}, crash "second")
	echo!(Str.inspect(t))
	Ok({})
}
