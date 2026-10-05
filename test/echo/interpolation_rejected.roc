# A `from_interpolation` that rejects a literal's segments fails the build: the
# conversion runs at compile time, so the program never starts.
Html := [Html(Str)].{
    from_interpolation : List(Str) -> Try((List(Str) -> Html), [InvalidInterpolation(Str)])
    from_interpolation = |segments|
        if segments.any(|segment| segment.contains("<script")) {
            Err(InvalidInterpolation("Html literals can't contain script tags"))
        } else {
            Str.from_interpolation(segments).map_ok(|assemble| |values| Html(assemble(values)))
        }

    to_str : Html -> Str
    to_str = |Html(s)| s
}

render : Str -> Html
render = |body| "<script>${body}</script>"

main! = |_| {
    echo!("started")
    echo!(render("alert(1)").to_str())
    Ok({})
}
