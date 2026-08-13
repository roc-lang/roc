# repro for https://github.com/roc-lang/roc/issues/10178
main! = |_| {
	values = [1, 2, 3, 4, 5, 6, 7]
	remaining = values.iter().drop_first(3)->List.from_iter()

	expect remaining == [4, 5, 6, 7]

	Ok({})
}
