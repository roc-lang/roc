app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11560
#
# The same unannotated `Try` match as app.roc, with a payload whose own hidden
# descriptor has to travel with the call. Boxy planning (`--specialize=no`)
# must map the worker's descriptor parameters onto the direct call's
# `Try(List(U64), U8)` representation and lower the app, like the default
# strategy already does; running it must then produce the correct result
# (`h(Ok([1, 2])) == [1, 2]`, so `main!` returns `Ok({})`).

h = |t| match t {
    Ok(x) => x
    _ => []
}

t : Try(List(U64), U8)
t = Ok([1, 2])

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    if h(t) == [1, 2] Ok({}) else Err(Exit(1))
}
