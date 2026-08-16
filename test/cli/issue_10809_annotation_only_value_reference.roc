# Repro for https://github.com/roc-lang/roc/issues/10809:
# a top-level declaration that has a type annotation and no value names no value
# at all, whatever its annotation says, so every reference to one is reported
# rather than handed to post-check lowering as a procedure use.
d : i

o = || {
    D() = d
}

n : U64

p = n

f : U64 -> U64

q = f(1)

add_n = |x| x + n

main! = |_| {
    _ = add_n(1)
    Ok({})
}
