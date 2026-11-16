# A list pattern nested in a record pattern reads its list for its length
# check and for its rest. The list is read through one binding, and every
# part lands where the pattern says.
main! = |args| {
	n = List.len(args)
	rows = [{ left: [n, 2, 3], right: [4] }, { left: [], right: [7, 8, 9] }]
	var $out = ""
	for { left: [.. as lefts], right: [.. as rights] } in rows {
		$out = Str.concat($out, "${Str.inspect(lefts)} ${Str.inspect(rights)}\n")
	}
	echo!($out)
	Ok({})
}
