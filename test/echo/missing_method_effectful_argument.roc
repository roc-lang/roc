# A call to a missing method runs its argument's effects before it crashes.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "receiver"
		n
	}.missing({
		echo!("arg effect\n")
		1
	})))
	Ok({})
}
