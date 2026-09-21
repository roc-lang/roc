# A generic function's open tag-union parameter keeps its row open, so a tag
# that only exists in the caller's row reaches the wildcard branch.
color_name : [Red, ..] -> Str
color_name = |color| match color {
	Red => "red"
	_ => "other"
}

main! = |_args| {
	echo!("${color_name(Blue)} ${color_name(Red)}\n")
	Ok({})
}
