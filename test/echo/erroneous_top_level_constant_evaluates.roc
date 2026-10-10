# An unannotated top-level constant bound to a call with an undefined argument
# is still evaluated at compile time, up to where it crashes, and reading it
# crashes.
f : U64, U64 -> U64
f = |a, b| a + b

value = f({
	dbg "constant"
	1
}, undefined_thing)

main! = |_args| {
	echo!("before\n")
	echo!(Str.inspect(value))
	Ok({})
}
