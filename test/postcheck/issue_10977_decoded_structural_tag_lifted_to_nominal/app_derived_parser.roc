app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import BarDerived

parse_fuzz : Str -> Try(BarDerived.Fuzz, _)
parse_fuzz = |input| {
    parsed : Try({ inner : BarDerived.Fuzz }, _)
    parsed = Json.parse(input)
    outer = parsed ? |e| Fizzled(e)
    Ok(outer.inner)
}

main! = |_args| {
    match parse_fuzz("{\"inner\":{\"alpha\":\"a\",\"beta\":\"Baz\"}}") {
        Ok(fuzz) => Stdout.line!(BarDerived.frob(fuzz))
        Err(_) => Stdout.line!("failed")
    }
    Ok({})
}
