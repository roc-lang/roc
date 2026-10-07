# A type-only method call on an unresolved type variable is rejected.
# Running anyway reaches the rejected call and crashes.
make : U64 -> a
make = |x| {
	A : a
	A.default({
		dbg "operand"
		x
	})
}

main! = |args| {
	echo!("before\n")
	_ = make(List.len(args))
	Ok({})
}
