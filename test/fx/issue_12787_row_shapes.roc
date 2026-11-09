app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Model : { a : [A(U64), B(Str)], n : U64 }

Doc : { x : F64 }

good = "{\"a\":{\"A\":3},\"n\":4}"

no_field = "{\"a\":{\"B\":\"text\"}}"

Closed : [InvalidJson(Str)]

Widened : [InvalidJson(Str), Rejected(Str), Retry(U64)]

parse_closed : Str -> Try(Model, Closed)
parse_closed = |json| Json.parse(json)

parse_widened : Str -> Try(Model, Widened)
parse_widened = |json| {
    if json == "rejected" {
        Err(Rejected("rejected before parsing: long enough for the heap"))
    } else if json == "retry" {
        Err(Retry(9))
    } else {
        Json.parse(json)
    }
}

parse_open : Str -> Try(Model, [InvalidJson(Str), ..others])
parse_open = |json| Json.parse(json)

parse_generic = |json| Json.parse(json)

encode_closed : Doc -> Try(Str, [Infinity, NaN, NegativeInfinity])
encode_closed = |doc| Json.to_str_try(doc)

encode_widened : Doc -> Try(Str, [Infinity, NaN, NegativeInfinity, Rejected(Str), Retry(U64)])
encode_widened = |doc| {
    if doc.x == 2 {
        Err(Rejected("rejected before encoding: long enough for the heap"))
    } else if doc.x == 3 {
        Err(Retry(9))
    } else {
        Json.to_str_try(doc)
    }
}

encode_open : Doc -> Try(Str, [Infinity, NaN, NegativeInfinity, ..others])
encode_open = |doc| Json.to_str_try(doc)

encode_generic = |doc| Json.to_str_try(doc)

# `?` leaves the error rows of these two open, and each caller's row sorts tags
# before and after the codec's own and gives one a larger payload.
parse_question = |json| {
    parsed : Model
    parsed = Json.parse(json)?
    Ok(parsed.n)
}

encode_question = |doc| {
    encoded : Str
    encoded = Json.to_str_try(doc)?
    Ok(Str.count_utf8_bytes(encoded))
}

QuestionParse : [Aardvark(Str, Str, U64), InvalidJson(Str), Zebra]

QuestionEncode : [Aardvark(Str, Str, U64), Infinity, NaN, NegativeInfinity, Zebra]

question_parse_row : Str -> Try(U64, QuestionParse)
question_parse_row = |json| {
    if json == "aardvark" {
        Err(Aardvark("first", "second", 3))
    } else if json == "zebra" {
        Err(Zebra)
    } else {
        parse_question(json)
    }
}

question_encode_row : Doc -> Try(U64, QuestionEncode)
question_encode_row = |doc| {
    if doc.x == 2 {
        Err(Aardvark("first", "second", 3))
    } else if doc.x == 3 {
        Err(Zebra)
    } else {
        encode_question(doc)
    }
}

# A format whose error type is a `Str` rather than a tag union.
StrFormat := [Default].{
    rename_field : StrFormat, Str -> Str
    rename_field = |_, name| name

    invalid_value : StrFormat, U64 -> Str
    invalid_value = |_, _| "invalid value"

    parse_tuple_start : StrFormat, U64, U64 -> Try(U64, Str)
    parse_tuple_start = |_, state, _| Ok(state)

    parse_tuple_next : StrFormat, U64, U64, U64 -> Try(U64, Str)
    parse_tuple_next = |_, state, _, _| Ok(state)

    parse_tuple_end : StrFormat, U64, U64 -> Try(U64, Str)
    parse_tuple_end = |_, state, _| Ok(state)

    parse_u64 : StrFormat, U64 -> Try({ value : U64, rest : U64 }, Str)
    parse_u64 = |_, state| if state == 7 { Err("seven") } else { Ok({ value: state, rest: state + 1 }) }

    encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)
    encode_record = |state, _, write_fields| {
        write_fields(state, |field_state, _, write_value| write_value(field_state))
    }

    encode_u64 : U64, U64 -> Try(U64, Str)
    encode_u64 = |value, state| if value == 0 { Err("zero") } else { Ok(state + value) }
}

str_parse : U64 -> Try({ value : a, rest : U64 }, err)
    where [a.parser_for : StrFormat -> (U64 -> Try({ value : a, rest : U64 }, err))]
str_parse = |state| {
    Shape : a
    parse_shape = Shape.parser_for(StrFormat.Default)
    parse_shape(state)
}

str_encode : value -> Try(U64, err)
    where [value.encoder_for : StrFormat -> (value, U64 -> Try(U64, err))]
str_encode = |value| {
    Shape : value
    encode_value = Shape.encoder_for(StrFormat.Default)
    encode_value(value, 1)
}

show_closed : Try(Model, Closed) -> Str
show_closed = |result| {
    match result {
        Ok({ a: A(value), n }) => "ok A ${value.to_str()} ${n.to_str()}"
        Ok({ a: B(text), n }) => "ok B ${text} ${n.to_str()}"
        Err(InvalidJson(message)) => message
    }
}

show_widened : Try(Model, Widened) -> Str
show_widened = |result| {
    match result {
        Ok({ a: A(value), n }) => "ok A ${value.to_str()} ${n.to_str()}"
        Ok({ a: B(text), n }) => "ok B ${text} ${n.to_str()}"
        Err(InvalidJson(message)) => message
        Err(Rejected(reason)) => reason
        Err(Retry(count)) => "retry ${count.to_str()}"
    }
}

show_encoded : Try(Str, [Infinity, NaN, NegativeInfinity]) -> Str
show_encoded = |result| {
    match result {
        Ok(json) => json
        Err(Infinity) => "infinity"
        Err(NaN) => "not a number"
        Err(NegativeInfinity) => "negative infinity"
    }
}

show_encoded_wide : Try(Str, [Infinity, NaN, NegativeInfinity, Rejected(Str), Retry(U64)]) -> Str
show_encoded_wide = |result| {
    match result {
        Ok(json) => json
        Err(Infinity) => "infinity"
        Err(NaN) => "not a number"
        Err(NegativeInfinity) => "negative infinity"
        Err(Rejected(reason)) => reason
        Err(Retry(count)) => "retry ${count.to_str()}"
    }
}

show_question_parse : Try(U64, QuestionParse) -> Str
show_question_parse = |result| {
    match result {
        Ok(n) => "ok ${n.to_str()}"
        Err(Aardvark(first, second, count)) => "aardvark ${first} ${second} ${count.to_str()}"
        Err(InvalidJson(message)) => message
        Err(Zebra) => "zebra"
    }
}

show_question_encode : Try(U64, QuestionEncode) -> Str
show_question_encode = |result| {
    match result {
        Ok(n) => "ok ${n.to_str()}"
        Err(Aardvark(first, second, count)) => "aardvark ${first} ${second} ${count.to_str()}"
        Err(Infinity) => "infinity"
        Err(NaN) => "not a number"
        Err(NegativeInfinity) => "negative infinity"
        Err(Zebra) => "zebra"
    }
}

show_str_parse : Try({ value : (U64, U64), rest : U64 }, Str) -> Str
show_str_parse = |result| {
    match result {
        Ok(parsed) => "ok ${parsed.value.0.to_str()} ${parsed.value.1.to_str()} ${parsed.rest.to_str()}"
        Err(message) => "err ${message}"
    }
}

show_str_encode : Try(U64, Str) -> Str
show_str_encode = |result| {
    match result {
        Ok(state) => "ok ${state.to_str()}"
        Err(message) => "err ${message}"
    }
}

main! = || {
    bad = "not json"
    Stdout.line!(show_closed(parse_closed(good)))
    Stdout.line!(show_closed(parse_closed(bad)))
    Stdout.line!(show_closed(parse_closed(no_field)))

    Stdout.line!(show_widened(parse_widened(good)))
    Stdout.line!(show_widened(parse_widened(bad)))
    Stdout.line!(show_widened(parse_widened(no_field)))
    Stdout.line!(show_widened(parse_widened("rejected")))
    Stdout.line!(show_widened(parse_widened("retry")))

    Stdout.line!(show_widened(parse_open(good)))
    Stdout.line!(show_widened(parse_open(bad)))
    Stdout.line!(show_widened(parse_open(no_field)))
    Stdout.line!(show_closed(parse_open(good)))
    Stdout.line!(show_closed(parse_open(no_field)))

    Stdout.line!(show_widened(parse_generic(good)))
    Stdout.line!(show_widened(parse_generic(bad)))
    Stdout.line!(show_closed(parse_generic(no_field)))

    one = 1.F64
    zero = 0.F64
    Stdout.line!(show_encoded(encode_closed({ x: one })))
    Stdout.line!(show_encoded(encode_closed({ x: zero / zero })))
    Stdout.line!(show_encoded_wide(encode_widened({ x: one })))
    Stdout.line!(show_encoded_wide(encode_widened({ x: one / zero })))
    Stdout.line!(show_encoded_wide(encode_widened({ x: 2 })))
    Stdout.line!(show_encoded_wide(encode_widened({ x: 3 })))
    Stdout.line!(show_encoded_wide(encode_open({ x: one })))
    Stdout.line!(show_encoded_wide(encode_open({ x: zero / zero })))
    Stdout.line!(show_encoded(encode_open({ x: one / zero })))
    Stdout.line!(show_encoded(encode_generic({ x: one })))
    Stdout.line!(show_encoded_wide(encode_generic({ x: zero / zero })))

    Stdout.line!(show_question_parse(question_parse_row(good)))
    Stdout.line!(show_question_parse(question_parse_row(bad)))
    Stdout.line!(show_question_parse(question_parse_row(no_field)))
    Stdout.line!(show_question_parse(question_parse_row("aardvark")))
    Stdout.line!(show_question_parse(question_parse_row("zebra")))
    Stdout.line!(show_question_encode(question_encode_row({ x: one })))
    Stdout.line!(show_question_encode(question_encode_row({ x: zero / zero })))
    Stdout.line!(show_question_encode(question_encode_row({ x: one / zero })))
    Stdout.line!(show_question_encode(question_encode_row({ x: 2 })))
    Stdout.line!(show_question_encode(question_encode_row({ x: 3 })))

    Stdout.line!(show_str_parse(str_parse(2)))
    Stdout.line!(show_str_parse(str_parse(6)))
    Stdout.line!(show_str_encode(str_encode({ n: 3.U64 })))
    Stdout.line!(show_str_encode(str_encode({ n: 0.U64 })))
}
