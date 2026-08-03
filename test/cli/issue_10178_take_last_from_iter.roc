# repro for https://github.com/roc-lang/roc/issues/10178
main! = |_| {
	values = [1, 2, 3, 4, 5, 6, 7]
	last_four = values.iter().take_last(4)->List.from_iter()

	expect last_four == [4, 5, 6, 7]

	Ok({})
}
