# A record field whose value crashes runs after the fields written before it
# and before the fields written after it.
main! = |args| {
	n = List.len(args)
	echo!("before\n")
	r = { zz: {
		dbg "zz"
		n
	}, aa: {
		crash "aa crashed"
	}, mm: {
		dbg "mm"
		n
	} }
	echo!("${Str.inspect(r)}\n")
	Ok({})
}
