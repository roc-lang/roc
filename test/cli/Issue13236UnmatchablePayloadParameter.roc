# Repro for https://github.com/roc-lang/roc/issues/13236: `Zero` is never
# produced, so its branch can never match, and `f` stays `a -> a`.
f = |n| match Oops {
    Oops => n
    Zero(p) => p
}

expect f(1.U64) == 1.U64
expect f("a") == "a"
