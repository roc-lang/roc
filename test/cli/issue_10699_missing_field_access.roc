# repro for https://github.com/roc-lang/roc/issues/10699
MissingFieldAccess :: [].{
	total : List(U64) -> U64
	total = |items| {
		record = { alpha: items.len(), beta: 2 }
		record.gamma
	}
}

expect MissingFieldAccess.total([1, 2, 3]) > 0
