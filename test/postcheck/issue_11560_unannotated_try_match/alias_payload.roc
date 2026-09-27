app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11560
#
# The unannotated `Try` match, called with a value whose type is an alias of
# `Try`. The callee names the tag union structure where the call names the
# alias of the nominal wrapping it, so Boxy planning (`--specialize=no`) sees
# through both wrappers to align the callee's descriptors with the call's
# `List(U64)` payload; running it must produce `h(Ok([4])) == [4]`.

MyTry : Try(List(U64), U8)

h = |t| match t {
    Ok(x) => x
    _ => []
}

t : MyTry
t = Ok([4])

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    if h(t) == [4] Ok({}) else Err(Exit(1))
}
