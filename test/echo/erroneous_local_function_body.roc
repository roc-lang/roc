# A local function whose body is a call with an undefined argument is not
# itself rejected, so defining it does nothing: calling it evaluates the
# call's earlier operands, then crashes.
f : U64, U64 -> U64
f = |a, b| a + b

main! = |args| {
	g = |n| f({
		dbg "arg"
		n
	}, undefined_thing)
	echo!("before\n")
	echo!(Str.inspect(g(List.len(args))))
	Ok({})
}
