# A generic function's open tag-union parameter keeps its row open, so a tag
# that only exists in the caller's row reaches the wildcard branch, including
# when the argument is a closed-union record field read at runtime.
color_name : [Red, ..] -> Str
color_name = |color| match color {
	Red => "red"
	_ => "other"
}

field_color_name : { c : [Red, Green], n : Str } -> Str
field_color_name = |r| Str.concat(color_name(r.c), r.n)

main! = |args| {
	suffix = Str.join_with(args.drop_first(1), "")
	long = Str.concat(" and a heap allocated string that is long", suffix)
	echo!("${color_name(Blue)} ${color_name(Red)} ${field_color_name({ c: Green, n: long })}\n")
	Ok({})
}
