# A call whose later argument always crashes evaluates its earlier argument,
# including the `dbg` inside it, before that crash.
f : U64, U64 -> U64
f = |a, b| a + b

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect(f({
		dbg "first"
		n
	}, crash "second")))
	Ok({})
}
