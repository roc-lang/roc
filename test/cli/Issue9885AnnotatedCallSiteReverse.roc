# repro for https://github.com/roc-lang/roc/issues/9885, call-site-annotated
# variant. Annotating the *call site* `List(I64)` (the def stays unannotated)
# flipped the unsound `List(a) -> List(b)` scheme into a compiler panic
# ("instantiation unified two different primitive types",
# src/postcheck/monotype/solve.zig). Must run cleanly.
main! = |_| {
    result : List(I64)
    result = reverse([1, 2, 3, 4])
    dbg result
    Ok({})
}

reverse = |list| {
    match list {
        [] => []
        [first, .. as rest] => reverse(rest).append(first)
    }
}
