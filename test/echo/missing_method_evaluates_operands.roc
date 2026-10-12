# A call to a method the receiver's type does not have evaluates its receiver
# and its arguments, then crashes.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "receiver"
		n
	}.not_a_method({
		dbg "arg"
		1
	})))
	Ok({})
}
