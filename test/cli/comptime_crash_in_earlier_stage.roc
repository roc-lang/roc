divide : I64 -> I64
divide = |denominator| 12.I64 / denominator

# `divide(0.I64)` is a compile-time value of its own, evaluated in an earlier
# stage than `value`, which reads it.
guarded : U64 -> I64
guarded = |index| {
    if index == 0 divide(0.I64) else 1
}

EarlierStage := [].{
    value : I64
    value = guarded(0)
}
