app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11560
#
# An opaque nominal is related to its backing's structure inside the module
# that declares it. `h` is unannotated and destructures a record, and is
# called with the opaque `Pt(List(Str))`; Boxy planning (`--specialize=no`)
# aligns the callee's record descriptors with the call's backing record, and
# running it must produce `h(p) == ["hi"]`.

Pt(a) :: { x : a, y : U64 }

h = |p| match p {
    { x, y: 2 } => x
    _ => []
}

p : Pt(List(Str))
p = { x: ["hi"], y: 2 }

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    if h(p) == ["hi"] Ok({}) else Err(Exit(1))
}
