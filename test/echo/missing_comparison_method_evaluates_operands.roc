# `<` on records, which have no `is_lt` method, evaluates both operands, then
# crashes.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "lhs"
		{ a: n }
	} < {
		dbg "rhs"
		{ a: 1 }
	}))
	Ok({})
}
