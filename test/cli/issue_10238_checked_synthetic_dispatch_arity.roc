# Repro for https://github.com/roc-lang/roc/issues/10238: an unresolved
# dispatch reports MISSING METHOD and lowers to an ordinary Roc crash.
g : (a -> a) -> I64 where [a.d : I64 -> a, a.encode : a -> I64]
g = |f| {
    A : a
    (f(A.d(41))).encode()
}

main! = |_| Ok(g(|n| n + 1))
