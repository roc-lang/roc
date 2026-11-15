# repro for https://github.com/roc-lang/roc/issues/13003
# A literal that `from_interpolation` rejects is a compile-time error whether
# or not anything calls the function holding it, exactly as a literal that
# `from_quote` rejects is: `roc check` reports both literals below once each.
Path :: { text : Str }.{
    from_quote : Str -> Try(Path, [BadQuotedBytes(Str)])
    from_quote = |raw| if raw.starts_with("/") Ok({ text: raw }) else Err(BadQuotedBytes("a quoted path starts with /"))

    from_interpolation : List(Str) -> Try((Iter(Str) -> Path), [InvalidInterpolation(Str)])
    from_interpolation = |segments|
        if (segments.first() ?? "").starts_with("/") {
            Str.from_interpolation(segments).map_ok(|assemble| |values| { text: assemble(values) })
        } else {
            Err(InvalidInterpolation("an interpolated path starts with /"))
        }
}

quoted : {} -> Path
quoted = |_| "no-slash"

interpolated : Str -> Path
interpolated = |name| "${name}/api"

main! = |_args| {
    echo!("hi")
    Ok({})
}
