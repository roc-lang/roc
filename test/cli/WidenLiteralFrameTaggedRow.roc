# Every row has a discriminant, and adding Err before Ok changes its index.
# A forwarded literal-frame dictionary must therefore adapt even when both
# rows have the same payload layout.
WidenLiteralFrameTaggedRow := {}

Job := [Pending].{
    label : () -> [Ok(Str), Z]
    label = || Ok("cv")
}

show : [A, Err(Str), Ok(Str), Z] -> Str
show = |value| match value {
    A => "A"
    Err(s) => "Err(${s})"
    Ok(s) => "Ok(${s})"
    Z => "Z"
}

Wrapper(a) := [Wrapped(Str)].{
    from_numeral : Numeral -> Try(Wrapper(a), [InvalidNumeral(Str)]) where [a.label : () -> [A, Err(Str), Ok(Str), Z]]
    from_numeral = |_| {
        Shape : a
        Ok(Wrapper.Wrapped(show(Shape.label())))
    }

    text : Wrapper(a) -> Str
    text = |Wrapper.Wrapped(s)| s
}

Nested(a) := [Nested(Str)].{
    convert : Numeral -> Try(Nested(a), [InvalidNumeral(Str)]) where [a.label : () -> [Err(Str), Ok(Str), Z]]
    convert = |_| {
        inner : Wrapper(a)
        inner = 7
        Ok(Nested.Nested(inner.text()))
    }

    from_numeral : Numeral -> Try(Nested(a), [InvalidNumeral(Str)]) where [a.label : () -> [Ok(Str), Z]]
    from_numeral = convert

    text : Nested(a) -> Str
    text = |Nested.Nested(s)| s
}

nested : a -> Nested(a) where [a.label : () -> [Ok(Str), Z]]
nested = |_| 5

expect nested(Job.Pending).text() == "Ok(cv)"
