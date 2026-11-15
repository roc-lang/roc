# Repro for https://github.com/roc-lang/roc/issues/10430: logical negation of
# an unannotated tag binding resolves the tag union at Bool and calls Bool.not.
go : U64 -> U64
go = |n| {
	done = False

	if !done {
		n
	} else {
		0
	}
}

expect go(1) == 1
