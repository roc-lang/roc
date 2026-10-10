# A record update evaluates its base, then its updated fields in the order
# written.
main! = |args| {
	n = List.len(args)
	base = { aa: n, mm: n, zz: n }
	r = { ..{
		dbg "base"
		base
	}, zz: {
		dbg "zz"
		n + 1
	}, aa: {
		dbg "aa"
		n + 2
	} }
	echo!("${Str.inspect(r)}\n")
	Ok({})
}
