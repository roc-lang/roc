# A list with reserved spare capacity keeps that capacity across an erased
# boundary: append after the round-trip grows in place via the unsafe append
# path, which reads the capacity the boundary conversion reported.
identity : a -> a
identity = |x| x

main! = |_args| {
	base = List.reserve([1, 2, 3], 8)
	through = identity(base)
	grown = through.append(4).append(5)
	echo!("${Str.join_with(grown.map(|n| n.to_str()), ",")}\n")
	Ok({})
}
