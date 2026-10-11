# A lambda passed as an argument whose body is a call with an undefined
# argument is not itself rejected: calling it evaluates the call's earlier
# operands, then crashes.
f : U64, U64 -> U64
f = |a, b| a + b

apply : (U64 -> U64), U64 -> U64
apply = |h, x| h(x)

main! = |args| {
	echo!("before\n")
	echo!(Str.inspect(apply(|n| f({
		dbg "arg"
		n
	}, undefined_thing), List.len(args))))
	Ok({})
}
