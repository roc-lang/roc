# Nested record literals evaluate their fields in the order written, each
# nested record's fields before the fields that follow it.
main! = |args| {
	n = List.len(args)
	r = { outer_z: { inner_z: {
		dbg "inner_z"
		n
	}, inner_a: {
		dbg "inner_a"
		n
	} }, outer_a: {
		dbg "outer_a"
		n
	} }
	echo!("${Str.inspect(r)}\n")
	Ok({})
}
