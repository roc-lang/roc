app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11560
#
# The unannotated `Try`-shaped match, called with an app-declared transparent
# nominal. The callee's `==` evidence path names the `Ok` payload of its tag
# union structure, which Boxy planning (`--specialize=no`) follows through the
# call's `Res(U64)` backing; running it must produce `h(Ok(3)) == 3`.

Res(a) := [Ok(a), Err(U8)]

h = |t| match t {
    Ok(x) => x
    _ => 0
}

t : Res(U64)
t = Ok(3)

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    if h(t) == 3 Ok({}) else Err(Exit(1))
}
