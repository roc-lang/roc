# A method call whose argument always crashes evaluates its receiver,
# including the `dbg` inside it, before that crash.
Box2 := [B(U64)].{
	add : Box2, U64 -> U64
	add = |B(a), b| a + b
}

main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "first"
		Box2.B(n)
	}.add(crash "second")))
	Ok({})
}
