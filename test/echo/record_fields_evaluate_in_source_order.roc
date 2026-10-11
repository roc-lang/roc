# A record literal whose fields are written out of label order evaluates them
# in the order written.
main! = |args| {
	n = List.len(args)
	r = { zz: {
		dbg "zz"
		n
	}, aa: {
		dbg "aa"
		n + 1
	}, mm: {
		dbg "mm"
		n + 2
	} }
	echo!("${Str.inspect(r)}\n")
	Ok({})
}
