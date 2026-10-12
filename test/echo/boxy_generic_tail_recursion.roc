## A generic self tail call whose result is described at runtime runs as a
## loop, so deep recursion does not grow the stack.

last_of : List(a), a, U64 -> a
last_of = |list, fallback, i| match List.get(list, i) {
	Ok(x) => last_of(list, x, i + 1)
	Err(_) => fallback
}

main! = |args| {
	n = 6000 + List.len(args)
	echo!(Str.join_with([last_of(List.repeat(3.U64, n), 7, 0).to_str(), last_of(List.repeat("s", n), "none", 0), last_of([], "none", 0)], ","))
	Ok({})
}
