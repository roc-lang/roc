# https://github.com/roc-lang/roc/issues/12936
# Restoring a closure must replay each scope's dispatches in that scope's frame.
Issue12936StoredClosureEvidence := {}

take_by_sum : U64, U64 -> (List(Str) -> List(Str))
take_by_sum = |x, y| {
	joined = |a, b| {
		read = |outcomes| outcomes.take_first(a)
		_sum = a + b
		read
	}
	joined(x, y)
}

first_one = take_by_sum(1, 1)
expect first_one(["x", "y"]) == ["x"]
