import Bar

Parse := [].{
    parse_fuzz : Str -> Try(Bar.Fuzz, _)
    parse_fuzz = |input| {
        parsed : Try({ inner : Bar.Fuzz }, _)
        parsed = Json.parse(input)
        outer = parsed ? |e| Fizzled(e)
        Ok(outer.inner)
    }
}
