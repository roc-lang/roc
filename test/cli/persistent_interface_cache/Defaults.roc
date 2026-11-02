Defaults := { count : U64 ?? 0 }.{
    parser_for : _
    is_eq : _
    make = |_| { value: 0.U64, base: Defaults.{} }

    parses_defaults = |_| {
        first : Try(Defaults, [InvalidJson(Str)])
        first = Json.parse("{}")
        second : Try(Other, [InvalidJson(Str)])
        second = Json.parse("{}")
        first == Ok(Defaults.{ count: 0 }) and second == Ok(Other.{ count: 3 })
    }
}

Other := { count : U64 ?? 3 }.{
    parser_for : _
    is_eq : _
}
