# An arithmetic operation whose right operand diverges evaluates its left
# operand, then the right one, which crashes.
total : U64 -> U64
total = |left| {
	dbg left
	left
} + { crash "binop operand diverged" }

main! = |args| {
	echo!("started")
	echo!(total(args.len()).to_str())
	Ok({})
}
