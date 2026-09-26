app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11560
#
# `h` is unannotated and matches on a `Try`, and `main!` calls `h` with a
# `Try(U64, U8)`. Boxy planning (`--specialize=no`) must materialize that
# direct call's hidden descriptor arguments and lower the app, like the
# default strategy already does; running it must then produce the correct
# result (`h(Ok(3)) == 3`, so `main!` returns `Ok({})`).

h = |t| match t {
    Ok(x) => x
    _ => 0
}

t : Try(U64, U8)
t = Ok(3)

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    if h(t) == 3 Ok({}) else Err(Exit(1))
}
