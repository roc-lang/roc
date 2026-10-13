# A list that is a static literal on one branch and a fresh list on the other
# is not unique, so appending to it must not write into the static data.
line_for : U64 -> List(U8)
line_for = |n| {
	suffix = if n == 0 [48] else List.repeat(49, n)
	suffix.append(33)
}

main! = |args| {
	echo!(Str.from_utf8_lossy(line_for(args.len() // 100)))
	echo!(Str.from_utf8_lossy(line_for(args.len() + 1)))
	Ok({})
}
