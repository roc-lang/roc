# Repro for https://github.com/roc-lang/roc/issues/11389.
app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! = |_args| Ok({})
