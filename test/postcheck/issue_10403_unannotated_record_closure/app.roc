app [program] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/10403
# An unannotated closure in a record may combine a method call and a literal.
program = { run: run }

run : Str -> Str
run = |_s| {
    _io = {
        write: |bytes| bytes.len() + 1,
    }

    "ok"
}
