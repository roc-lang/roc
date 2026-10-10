# An unannotated top-level constant that always crashes binds nothing: using
# it as a number adds no report, it crashes once at compile time after its
# `dbg`, and the use crashes.
value = {
	dbg "constant"
	crash "boom"
}

main! = |args| {
	echo!("before\n")
	echo!(Str.inspect(value + List.len(args)))
	Ok({})
}
