# An unannotated function whose body is a call with an undefined argument is
# not itself rejected: calling it evaluates the call's earlier operands, then
# crashes.
f : U64, U64 -> U64
f = |a, b| a + b

g = |n| f({
	dbg "arg"
	n
}, undefined_thing)

main! = |args| {
	echo!("before\n")
	echo!(Str.inspect(g(List.len(args))))
	Ok({})
}
