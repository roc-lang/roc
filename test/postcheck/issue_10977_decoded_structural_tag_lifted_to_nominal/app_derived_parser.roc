app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import BarDerived

parse_fuzz : Str -> Try(BarDerived.Fuzz, _)
parse_fuzz = |input| {
    parsed : Try({ inner : BarDerived.Fuzz }, _)
    parsed = Json.parse(input)
    outer = parsed ? |e| Fizzled(e)
    Ok(outer.inner)
}

main! = |_args| {
    fuzz = parse_fuzz("{\"inner\":{\"beta\":\"Baz\"}}")?
    _ = BarDerived.frob(fuzz)
    Ok({})
}
