# repro for https://github.com/roc-lang/roc/issues/13015
# `view`'s literal converts at `M(U64)`, the type `main!`'s call fixes, and
# `from_interpolation` rejects it there: `roc check` reports the rejection at
# the literal once, as `roc build` does.
M(a) := { count : U64, tag : List(a) }.{
    from_interpolation : List(Str) -> Try((Iter(Str) -> M(a)), [InvalidInterpolation(Str)])
    from_interpolation = |segments|
        if segments.any(|s| s.contains("bad")) {
            Err(InvalidInterpolation("rejected: the literal holds bad"))
        } else {
            Ok(|parts| { count: List.from_iter(parts).len(), tag: [] })
        }
}

view : Str -> M(a)
view = |x| "bad ${x}"

main! = |args| {
    m : M(U64)
    m = view(args.len().to_str())
    echo!(m.count.to_str())
    Ok({})
}
