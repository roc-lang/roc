# A record literal whose fields are written in label order evaluates them in
# that order.
main! = |args| {
	n = List.len(args)
	r = { aa: {
		dbg "aa"
		n
	}, bb: {
		dbg "bb"
		n
	}, cc: {
		dbg "cc"
		n
	} }
	echo!("${Str.inspect(r)}\n")
	Ok({})
}
