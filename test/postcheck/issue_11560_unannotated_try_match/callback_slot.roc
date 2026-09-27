app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11560
#
# `run` matches on the `Try` its callback returns. The local closure `g` is
# unannotated, so its result is a structural row where `run`'s callback slot
# names `Try`; Boxy (`--specialize=no`) aligns the callable's descriptors
# through that `Try` at the call and in its callable adapter. Running it
# must produce `run(g, 7) == [7]` and `run(mk, 9) == [9]`.

mk : U64 -> Try(List(U64), [Neg])
mk = |n| if n > 5 Ok([n]) else Err(Neg)

run : (U64 -> Try(List(U64), [Neg])), U64 -> List(U64)
run = |f, n| match f(n) {
    Ok(x) => x
    Err(_) => []
}

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    g = |n| if n > 5 Ok([n]) else Err(Neg)
    if run(g, 7) == [7] and run(mk, 9) == [9] Ok({}) else Err(Exit(1))
}
