app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost

parse_article : Str -> Try({ favorites_count : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
parse_article = |input| {
    parse : Str -> Try({ favorites_count : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
    parse = Json.parser_camel()
    parse(input)
}

main! : () => Str
main! = || {
    input = FallibleHost.json_input!({})

    match parse_article(input) {
        Ok(article) => if article.favorites_count == 14 { "14" } else { "wrong-value" }
        Err(MissingRequiredField(field)) => Str.concat("missing:", field)
        Err(InvalidJson(message)) => Str.concat("invalid:", message)
    }
}
