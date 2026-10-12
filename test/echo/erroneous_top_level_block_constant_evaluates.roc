# An unannotated top-level constant whose block ends in a call with an
# undefined argument is evaluated once, at compile time, up to where it
# crashes, and reading it crashes.
f : U64, U64 -> U64
f = |a, b| a + b

value = {
	dbg "constant"
	f(1, undefined_thing)
}

main! = |_args| {
	echo!("before\n")
	echo!(Str.inspect(value))
	Ok({})
}
