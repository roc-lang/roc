# A call with an undefined argument evaluates the closure passed before it,
# then crashes.
apply : (U64 -> U64), U64 -> U64
apply = |h, x| h(x)

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect(apply({
		dbg "closure"
		|x| x + n
	}, undefined_thing)))
	Ok({})
}
