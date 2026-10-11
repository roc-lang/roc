# A call to a missing method evaluates its receiver and the closure passed to
# it, then crashes.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "receiver"
		n
	}.missing(|x| x + n)))
	Ok({})
}
