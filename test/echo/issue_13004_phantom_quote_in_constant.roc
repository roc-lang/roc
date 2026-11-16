# repro for https://github.com/roc-lang/roc/issues/13004
# The literal's type `Sig(ctx)` has a parameter nothing determines. The
# top-level constant converts it and evaluates to "Save" at compile time, as
# the same call does inside `main!`.
Sig(ctx) :: [Const(Str)].{
    from_quote : Str -> Try(Sig(ctx), [BadQuotedBytes(Str)])
    from_quote = |raw| Ok(Const(raw))

    text : Sig(ctx) -> Str
    text = |s| match s {
        Const(t) => t
    }
}

a : Str
a = Sig.text("Save")

main! = |_args| {
    echo!(a)
    Ok({})
}
