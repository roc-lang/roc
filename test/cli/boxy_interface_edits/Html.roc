Html := [Html(Str)].{
    from_interpolation : List(Str) -> Try((Iter(Str) -> Html), [InvalidInterpolation(Str)])
    from_interpolation = |segments| Str.from_interpolation(segments).map_ok(|assemble| |values| Html(assemble(values)))

    to_str : Html -> Str
    to_str = |Html(s)| s
}
