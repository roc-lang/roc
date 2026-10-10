# A method call on a value whose type is an unresolved variable is rejected.
# Running anyway reaches the rejected call and crashes.
never : U64 -> a
never = |n| {
	dbg "receiver"
	if n > 1000 { crash "never: big" } else { crash "never: small" }
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	_ = never(n).frob({
		dbg "arg"
		n
	})
	Ok({})
}
