Html := [Html(Str)].{
    from_interpolation : List(Str) -> Try((Iter(Str) -> Html), [InvalidInterpolation(Str)])
    from_interpolation = |segments|
        if segments.any(|segment| segment.contains("<script")) {
            Err(InvalidInterpolation("Html literals can't contain script tags"))
        } else {
            Str.from_interpolation(segments).map_ok(|assemble| |values| Html(assemble(values)))
        }

    to_str : Html -> Str
    to_str = |Html(s)| s
}
