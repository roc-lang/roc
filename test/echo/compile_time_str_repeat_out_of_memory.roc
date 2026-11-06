# A count whose result no 32-bit target can address: compile-time evaluation
# crashes where the allocation is asked for, and the compilation reports it.
x = Str.repeat("ab", 4294967297)

main! = |_args| {
	echo!(Str.inspect(Str.count_utf8_bytes(x)))
	Ok({})
}
