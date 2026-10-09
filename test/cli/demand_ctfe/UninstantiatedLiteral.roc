app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout

Rejected(a) := { text : Str }.{
    from_quote : Str -> Try(Rejected(a), [BadQuotedBytes(Str)])
    from_quote = |_| Err(BadQuotedBytes("uninstantiated quote"))
}

# This generic definition is intentionally never given a concrete target.
unused : U64 -> Rejected(a)
unused = |_runtime| "uninstantiated literal"

main! = |_args| {
    Stdout.line!("runtime entrypoint")
    Ok({})
}
