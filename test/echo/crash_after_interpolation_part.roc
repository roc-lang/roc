# A string interpolation whose later part always crashes evaluates its earlier
# part, including the `dbg` inside it, before that crash.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	s : Str
	s = "${{
		dbg "interp"
		n.to_str()
	}} and ${crash "second"}"
	echo!(s)
	Ok({})
}
