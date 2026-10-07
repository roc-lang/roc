# A generic helper that calls a method on its argument, given a value whose
# type is an unresolved variable, is rejected. Running anyway crashes.
never : U64 -> a
never = |n| {
	dbg "receiver"
	if n > 1000 { crash "never: big" } else { crash "never: small" }
}

helper = |x, y| x.frob(y)

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	_ = helper(never(n), {
		dbg "arg"
		n
	})
	Ok({})
}
