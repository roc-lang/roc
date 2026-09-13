# Repro for https://github.com/roc-lang/roc/issues/10430: logical negation
# infers Bool for an unannotated tag without requiring a `not` method.
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
