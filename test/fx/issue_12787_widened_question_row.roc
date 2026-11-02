app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# `summarize`'s error row stays open, so its derived parser's sealed row is
# adapted to that open row, which this program's callers widen with tags that
# sort on both sides of the parser's own and with a larger payload.
Email := [Email(Str)].{
    parser_for = |encoding| {
        parse_str = Str.parser_for(encoding)
        |state| {
            parsed = parse_str(state)?
            if Str.contains(parsed.value, "@") {
                Ok({ value: Email.Email(parsed.value), rest: parsed.rest })
            } else {
                Err(InvalidEmail(parsed.value))
            }
        }
    }

    to_str = |Email.Email(address)| address
}

summarize = |json| {
    { name, email, num_stars } = Json.parse(json)?
    Ok(Str.concat(name, " <${Email.to_str(email)}> ${Str.repeat("*", num_stars)}"))
}

Row : [Aardvark(Str, Str, U64), InvalidEmail(Str), InvalidJson(Str), MissingRequiredField(Str), Zebra]

wide : Str -> Try(Str, Row)
wide = |json| {
    if json == "a" {
        Err(Aardvark("first", "second", 3))
    } else if json == "z" {
        Err(Zebra)
    } else {
        summarize(json)
    }
}

show : Try(Str, Row) -> Str
show = |result| {
    match result {
        Ok(text) => text
        Err(Aardvark(a, b, c)) => "aardvark ${a} ${b} ${c.to_str()}"
        Err(InvalidEmail(raw)) => "invalid email ${raw}"
        Err(InvalidJson(_)) => "invalid json"
        Err(MissingRequiredField(field)) => "missing ${field}"
        Err(Zebra) => "zebra"
    }
}

main! = || {
    Stdout.line!(show(wide("{\"name\":\"Ann\",\"email\":\"ann@example.com\",\"num_stars\":3}")))
    Stdout.line!(show(wide("{\"name\":\"Ann\",\"email\":\"not an email\",\"num_stars\":3}")))
    Stdout.line!(show(wide("{\"name\":\"Ann\",\"email\":\"ann@example.com\",\"num_stars\":-3}")))
    Stdout.line!(show(wide("{\"name\":\"Ann\",\"num_stars\":3}")))
    Stdout.line!(show(wide("a")))
    Stdout.line!(show(wide("z")))
}
