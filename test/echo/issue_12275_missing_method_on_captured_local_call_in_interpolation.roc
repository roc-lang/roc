# U32 has no `to_string`, so the method call is rejected and its interpolation
# with it. The rejected interpolation still evaluates the call to `add_offset`,
# a local closure capturing `offset`, before it crashes, so it must not be
# evaluated apart from the block that declares `add_offset`.
main! = |_args| {
	echo!("before")
	offset = 10.U32
	add_offset : U32 -> U32
	add_offset = |x| x + offset
	echo!("${add_offset(1.U32).to_string()}")
	Ok({})
}
