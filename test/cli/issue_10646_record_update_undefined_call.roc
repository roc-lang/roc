# repro for https://github.com/roc-lang/roc/issues/10646
app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! : List(Str) => Try({}, _)
main! = |_args| {
    r = { mode: 1 }
    _ = { ..r, mode: missing_function() }
    Ok({})
}
