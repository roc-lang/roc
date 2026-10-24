app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout

Inner(a) := { text : Str }.{
    from_quote : Str -> Try(Inner(a), [BadQuotedBytes(Str)])
    from_quote = |_| Err(BadQuotedBytes("nested demanded quote"))
}

prefix = "captured prefix"

convert : Str -> Inner(a)
convert = |raw| {
    nested : Inner(a)
    nested = "rejected nested quote"
    { text: Str.concat(prefix, Str.concat(raw, nested.text)) }
}

Outer(a) := { inner : Inner(a) }.{
    from_quote : Str -> Try(Outer(a), [BadQuotedBytes(Str)])
    from_quote = |raw| Ok({ inner: convert(raw) })
}

choose : U64 -> Outer(a)
choose = |_runtime| "outer quote"

main! = |args| {
    output : Outer(U64)
    output = choose(args.len().to_u64())
    Stdout.line!(output.inner.text)
    Ok({})
}
