app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout

# The runtime argument fixes a, but the match's scrutinee is CTFE-known.
observe : a -> U64
observe = |_runtime| {
    candidate : Try(a, Str)
    candidate = Err("specialized empirical failure")
    match candidate {
        Ok(_) => 42
    }
}

main! = |args| {
    Stdout.line!(observe(args.len().to_u64()).to_str())
    Ok({})
}
