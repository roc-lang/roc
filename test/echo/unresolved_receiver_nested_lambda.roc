# A method call on an unresolved receiver inside a local function is rejected.
# Running anyway crashes when that function is called.
never : U64 -> a
never = |n| {
	dbg "receiver"
	if n > 1000 { crash "never: big" } else { crash "never: small" }
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	f = |k| never(k).frob({
		dbg "inner"
		k
	})
	_ = f(n)
	Ok({})
}
