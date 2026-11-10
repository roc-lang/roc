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
            match Email.from_str(parsed.value) {
                Ok(email) => Ok({ value: email, rest: parsed.rest })
                Err(InvalidEmail(raw)) => Err(InvalidJson("Invalid email: ${raw}"))
            }
        }
    }

    from_str : Str -> Try(Email, [InvalidEmail(Str)])
    from_str = |raw| {
        if Str.contains(raw, "@") {
            Ok(Email(raw))
        } else {
            Err(InvalidEmail(raw))
        }
    }

    to_str = |Email.Email(address)| address
}

summarize = |json| {
    { name, email, num_stars } = Json.parse(json)?
    Ok(Str.concat(name, " <${Email.to_str(email)}> ${Str.repeat("*", num_stars)}"))
}

Row : [Aardvark(Str, Str, U64), InvalidJson(Str), Zebra]

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
        Err(InvalidJson(message)) => message
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
