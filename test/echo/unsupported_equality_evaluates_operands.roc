# `==` on functions, which do not support equality, evaluates both operands,
# then crashes.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "lhs"
		|x| x + n
	} == {
		dbg "rhs"
		|x| x + 1
	}))
	Ok({})
}
