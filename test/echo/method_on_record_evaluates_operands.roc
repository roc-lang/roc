# A method call on a record, which has no methods, evaluates its receiver and
# its arguments, then crashes.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	echo!(Str.inspect({
		dbg "receiver"
		{ a: n }
	}.whatever({
		dbg "arg"
		1
	})))
	Ok({})
}
