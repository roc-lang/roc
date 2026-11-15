# `dbg` inside a function prints the record it builds, and the tag union an
# exhaustive match closes, as each use has them: the nominal type a use makes
# them, with its inspect override, or the plain value (design.md "Methods On
# Open Rows").
Shown := { id : U64 }.{
	to_inspect : Shown -> Str
	to_inspect = |s| Str.concat("shown ", s.id.to_str())
}

Light := [Red, Green].{
	to_inspect : Light -> Str
	to_inspect = |_| "a light"
}

make_dbg = |id| {
	r = { id: id }
	dbg r
	r
}

name_dbg = |c| {
	dbg c
	match c {
		Red => "red"
		Green => "green"
	}
}

main! = |_args| {
	a : Shown
	a = make_dbg(1)
	b = make_dbg(2)
	l : Light
	l = Red
	echo!(Str.concat(name_dbg(l), name_dbg(Green)))
	echo!(Str.inspect((a, b.id)))
	Ok({})
}
