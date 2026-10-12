# Structural equality compares each component of its operands, but evaluates
# each operand exactly once, in order.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "lhs"
		(n, 1.U64, { a: 3.U64, b: "x" })
	} == {
		dbg "rhs"
		(n, 1.U64, { a: 3.U64, b: "x" })
	}))
	Ok({})
}
