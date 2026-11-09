# A procedure returning a list that is a static literal on one branch and a
# fresh list on the other does not return a unique list, so its caller's
# append must not write into the static data.
digits_for! : U64 => List(U8)
digits_for! = |n| {
	digits = if n == 0 [48] else List.repeat(49, n)
	echo!("d")
	digits
}

main! = |args| {
	echo!(Str.from_utf8_lossy(digits_for!(args.len() // 100).append(33)))
	echo!(Str.from_utf8_lossy(digits_for!(args.len() + 1).append(33)))
	Ok({})
}
