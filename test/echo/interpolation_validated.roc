# An interpolated string literal's `from_interpolation` sees only the literal's
# own segments, at compile time, and returns the function that assembles the
# interpolated values at runtime. A runtime value can therefore never be
# rejected: here it is escaped, while the segments were validated once.
Html := [Html(Str)].{
    from_interpolation : List(Str) -> Try((List(Str) -> Html), [InvalidInterpolation(Str)])
    from_interpolation = |segments|
        if segments.any(|segment| segment.contains("<script")) {
            Err(InvalidInterpolation("Html literals can't contain script tags"))
        } else {
            Str.from_interpolation(segments).map_ok(|assemble| |values| Html(assemble(List.map(values, |value| value.replace_each("<", "&lt;")))))
        }

    to_str : Html -> Str
    to_str = |Html(s)| s
}

# The target is chosen by each caller, so this converts through the caller's
# `from_interpolation`.
paragraph = |body| "<p>${body}</p>"

main! = |args| {
    html : Html
    html = paragraph("Roc & friends <3")
    echo!(html.to_str())
    str : Str
    str = paragraph("<plain>")
    echo!(str)
    user_input = if List.len(args) > 100 { "fine" } else { "<script>alert(1)</script>" }
    escaped : Html
    escaped = paragraph(user_input)
    echo!(escaped.to_str())
    Ok({})
}
