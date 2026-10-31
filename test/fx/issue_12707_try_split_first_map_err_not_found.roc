# Repro for https://github.com/roc-lang/roc/issues/12707
# `f` is unannotated: its `?` on `Str.split_first` and its `map_err` both
# produce `NotFound`, so `f`'s error row is `[NotFound]`. `roc check` accepts
# it, and building and running it must print `Ok("0")`.
app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!(Str.inspect(f("0 errors")))
}

f = |line| {
    at = line.split_first(" error")?
    Str.from_utf8(Str.to_utf8(at.before)).map_err(|_| NotFound)
}
