app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11560
#
# The unannotated `Try` match, reached through a record field. The callee's
# record holds an open tag row where the call's record holds `Try(Str, U8)`,
# so Boxy planning (`--specialize=no`) aligns both the descriptor and the
# dictionary parameters of that row through the call's `Try` backing, and
# running it must produce `h(r) == "hi"`.

h = |r| match r.t {
    Ok(x) => x
    _ => "none"
}

r : { t : Try(Str, U8), n : U64 }
r = { t: Ok("hi"), n: 1 }

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    if h(r) == "hi" Ok({}) else Err(Exit(1))
}
