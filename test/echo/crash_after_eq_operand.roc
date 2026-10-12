# Structural equality whose right operand always crashes evaluates its left
# operand, including the `dbg` inside it, before that crash.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	b = ({
		dbg "lhs"
		(n, n)
	} == crash "second")
	echo!(Str.inspect(b))
	Ok({})
}
