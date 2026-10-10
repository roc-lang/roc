# A record whose later field (in source order) always crashes evaluates its
# earlier field, including the `dbg` inside it, before that crash, even though
# the crashing field's label sorts first.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	r : { b : U64, a : U64 }
	r = { b: {
		dbg "first"
		n
	}, a: crash "second" }
	echo!(Str.inspect(r))
	Ok({})
}
