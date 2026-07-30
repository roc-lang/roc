# repro for https://github.com/roc-lang/roc/issues/10020
# An inline effectful lambda stored in a record field must keep the open Try
# error row inferred for that field when it is called.
app [main!] { pf: platform "../fx-open/platform/main.roc" }

import pf.Stdout

emit! : Str => Try({}, [EmitFailed, ..])
emit! = |s| {
    Stdout.line!(s)
    Ok({})
}

main! : List(Str) => Try({}, _)
main! = |_args| {
    rec = { shout!: |s| emit!("<<${s}>>") }
    (rec.shout!)("boom")
}
