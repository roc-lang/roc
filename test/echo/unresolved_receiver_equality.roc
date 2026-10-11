# Equality between values whose type is an unresolved variable is rejected.
# Running anyway reaches the rejected comparison and crashes.
never : U64 -> a
never = |n| {
	dbg "receiver"
	if n > 1000 { crash "never: big" } else { crash "never: small" }
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	b = never(n) == {
		dbg "rhs"
		never(n)
	}
	echo!(Str.inspect(b))
	Ok({})
}
