# repro for https://github.com/roc-lang/roc/issues/9885
# Unannotated recursive `reverse` must infer `List(a) -> List(a)`. The old
# deferred recursive-ref validation generalized the unsound
# `List(a) -> List(b)`; the unconstrained `b` defaulted to the empty tag union
# and running crashed with "uninhabited value reached Str.inspect" in `dbg`.
main! = |_| {
    result = reverse(["a", "b", "c", "d"])
    dbg result
    Ok({})
}

reverse = |list| {
    match list {
        [] => []
        [first, .. as rest] => reverse(rest).append(first)
    }
}
