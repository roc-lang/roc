# Run with: roc test main.roc --no-cache --specialize=no

# repro for https://github.com/roc-lang/roc/issues/11550
#
# Under Boxy lowering (`--specialize=no`), calling a generic function with a
# value whose type still contains an undetermined type variable panicked with
# "unresolved bare dynamic representation required a static descriptor". The
# `e` of `Ok(1)` is never constrained at the call site, and comparing the
# result leaves the checker's ownerless structural-equality placeholder on it,
# so the caller had no static descriptor for the hidden argument. Both calls
# must lower and every expect must pass.

identity : Try(U64, e) -> Try(U64, e)
identity = |t| t

expect identity(Ok(1)) == Ok(1)
expect Ok(1) == identity(Ok(1))

# The issue's reported input: the `e` of the `Ok` argument is never constrained.
unwrap : Try(a, e), a -> a
unwrap = |t, d| match t {
	Ok(x) => x
	_ => d
}

expect unwrap(Ok("inner string long enough to be heap allocated"), "d") == "inner string long enough to be heap allocated"