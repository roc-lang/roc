# A record literal in a generic function evaluates its fields in the order
# written at every instantiation.
pair : a, a -> { zz : a, aa : a }
pair = |x, y| { zz: {
	dbg "zz"
	x
}, aa: {
	dbg "aa"
	y
} }

main! = |args| {
	n = List.len(args)
	r = pair(n, n + 1)
	s = pair("x", "y")
	echo!("${Str.inspect(r)} ${Str.inspect(s)}\n")
	Ok({})
}
