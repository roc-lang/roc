# https://github.com/roc-lang/roc/issues/13186
Repro := [].{
    f : U64 -> U64
    f = |n| {
        expect n > 0
        n
    }
}

expect Repro.f(1) == 1
