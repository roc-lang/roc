Issue9975SpecConstrListTuplePatterns :: [].{}

# Issue 9975: every branch of this match starts with a list pattern inside a
# tuple pattern. Call-pattern specialization cannot decide those statically,
# so the match must stay residual instead of being treated as having no
# matching branch.
nth : List(a), U64 -> Try(a, [OutOfBounds])
nth = |l, i| {
	match (l, i) {
		([], _) => Err(OutOfBounds)
		([e, ..], 0) => Ok(e)
		([_, .. as rest], _) => nth(rest, (i - 1))
	}
}

expect nth(["a"], 0) == Ok("a")
expect nth(["a", "b", "c"], 2) == Ok("c")
expect nth(["a"], 1) == Err(OutOfBounds)
