Attr(msg) := [Handler(Str -> msg)].{
    map : Attr(a), (a -> b) -> Attr(b)
    map = |attr, f|
        match attr {
            Handler(cb) => Handler(|s| f(cb(s)))
        }

    island : Attr({})
    island = Attr.map(Handler(|_value| Typed), |_| {})
}
