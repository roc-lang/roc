# A nominal record literal evaluates its fields in the order written.
Point := { y : U64, x : U64 }

main! = |args| {
	n = List.len(args)
	p : Point
	p = { y: {
		dbg "y"
		n
	}, x: {
		dbg "x"
		n + 1
	} }
	echo!("${Str.inspect(p)}\n")
	Ok({})
}
