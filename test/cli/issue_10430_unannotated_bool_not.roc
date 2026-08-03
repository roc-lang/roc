# Repro for https://github.com/roc-lang/roc/issues/10430: this should report
# MISSING METHOD for `not` without panicking in post-check lowering.
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
