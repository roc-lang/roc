app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11560
#
# `h` is unannotated and reads a field, so its parameter is an open record
# where the call passes the transparent nominal `Pt(Str)`. Under Boxy
# (`--specialize=no`) the nominal's descriptor names its backing record's
# fields, so the value is received by field name and inspects as a record:
# `h(p) == "hi"` and `Str.inspect(p) == "{ x: \"hi\", y: 2 }"`.

Pt(a) := { x : a, y : U64 }

h = |r| r.x

p : Pt(Str)
p = { x: "hi", y: 2 }

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    if h(p) == "hi" and Str.inspect(p) == "{ x: \"hi\", y: 2 }" Ok({}) else Err(Exit(1))
}
