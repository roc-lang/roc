# A type module checked on its own validates every closed literal conversion
# in it, called or not: `roc check` reports the rejected interpolation in
# `api`, which nothing calls.
Issue13003Path :: { text : Str }.{
    from_interpolation : List(Str) -> Try((Iter(Str) -> Issue13003Path), [InvalidInterpolation(Str)])
    from_interpolation = |segments|
        if (segments.first() ?? "").starts_with("/") {
            Str.from_interpolation(segments).map_ok(|assemble| |values| { text: assemble(values) })
        } else {
            Err(InvalidInterpolation("a module path starts with /"))
        }

    api : Str -> Issue13003Path
    api = |name| "${name}/api"
}
