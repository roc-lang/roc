# Repro for https://github.com/roc-lang/roc/issues/11366: an inline `expect`
# whose condition has a type error is reported by the checker, exactly like an
# erroneous top-level `expect`, and blocks tests that reach it without executing them.
xs : List(U64)
xs = [1, 2, 3]

f = |n| {
    expect 3 >= xs.len
    n
}

expect f(1) == 1

expect Bool.True
