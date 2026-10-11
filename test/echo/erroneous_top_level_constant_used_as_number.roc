# An unannotated top-level constant bound to a call with an undefined argument
# binds nothing: using it as a number adds no report, its `dbg` runs once at
# compile time, and the use crashes.
f : U64, U64 -> U64
f = |a, b| a + b

value = f({
	dbg "constant"
	1
}, undefined_thing)

main! = |args| {
	echo!("before\n")
	echo!(Str.inspect(value + List.len(args)))
	Ok({})
}
