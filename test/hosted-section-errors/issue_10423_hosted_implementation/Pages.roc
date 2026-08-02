# Repro for https://github.com/roc-lang/roc/issues/10423
Pages := [].{
    list! : Str => Try({}, [Oops])
    list! = |_| {
        crash "unimplemented list!"
    }
}
