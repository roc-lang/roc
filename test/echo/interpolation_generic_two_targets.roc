# `label`'s interpolation converts at the target each call fixes: `Plain`
# accepts its segments and `Strict` rejects them, so checking reports the
# literal exactly once, for the `Strict` instantiation.
Plain := [Plain(Str)].{
    from_interpolation : List(Str) -> Try((Iter(Str) -> Plain), [InvalidInterpolation(Str)])
    from_interpolation = |segments| Str.from_interpolation(segments).map_ok(|assemble| |values| Plain(assemble(values)))

    text : Plain -> Str
    text = |Plain(s)| s
}

Strict := [Strict(Str)].{
    from_interpolation : List(Str) -> Try((Iter(Str) -> Strict), [InvalidInterpolation(Str)])
    from_interpolation = |_segments| Err(InvalidInterpolation("Strict literals take no values"))

    text : Strict -> Str
    text = |Strict(s)| s
}

label : Str -> t where [t.from_interpolation : List(Str) -> Try((Iter(Str) -> t), [InvalidInterpolation(Str)])]
label = |name| "<${name}>"

main! = |_args| {
    plain : Plain
    plain = label("p")
    strict : Strict
    strict = label("s")
    echo!(plain.text())
    echo!(strict.text())
    Ok({})
}
