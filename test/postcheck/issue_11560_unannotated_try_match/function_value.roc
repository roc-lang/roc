app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11560
#
# The top-level `m` is unannotated and matches on `Ok`, and is passed as a
# value into a callback slot that names `Try`. Boxy (`--specialize=no`)
# emits `m`'s erased worker at that instantiation, where its tag pattern's
# checked type is `m`'s structural row but its representation is `Try`, so
# the pattern descends `Try`'s backing while keeping the structural type.
# Running it must produce `apply(m, t) == [7, 8]`.

apply : (Try(List(U64), U8) -> List(U64)), Try(List(U64), U8) -> List(U64)
apply = |f, v| f(v)

m = |u| match u {
    Ok(x) => x
    _ => []
}

t : Try(List(U64), U8)
t = Ok([7, 8])

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    if apply(m, t) == [7, 8] Ok({}) else Err(Exit(1))
}
