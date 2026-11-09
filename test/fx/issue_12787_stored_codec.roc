app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Parsing uses the format-owned `[Negative]` error. Ordinary callers add
# Reset/Skipped after invoking the stored parser without widening its contract.
Format := [Default].{
    invalid_value : Format, U64 -> [Negative]
    invalid_value = |_, _| Negative

    parse_tuple_start : Format, U64, U64 -> Try(U64, [Negative])
    parse_tuple_start = |_, state, _| Ok(state)

    parse_tuple_next : Format, U64, U64, U64 -> Try(U64, [Negative])
    parse_tuple_next = |_, state, _, _| Ok(state)

    parse_tuple_end : Format, U64, U64 -> Try(U64, [Negative])
    parse_tuple_end = |_, state, _| Ok(state)

    parse_u64 : Format, U64 -> Try({ value : U64, rest : U64 }, [Negative])
    parse_u64 = |_, state| if state == 7 { Err(Negative) } else { Ok({ value: state, rest: state + 1 }) }

    encode_u64 : U64, U64 -> Try(U64, [Zero])
    encode_u64 = |value, state| if value == 0 { Err(Zero) } else { Ok(state + value) }
}

Pair : (U64, U64)

Wide : [Negative, Reset(Str), Skipped(U64)]

WideEnc : [Zero, Reset(Str), Skipped(U64)]

# Each use checks its own deferred codec relation against the format.
any_parser = |_| {
    Shape : Pair
    Shape.parser_for(Format.Default)
}

any_encoder = |_| {
    Shape : U64
    Shape.encoder_for(Format.Default)
}

parser_of : shape -> (U64 -> Try({ value : shape, rest : U64 }, err))
    where [shape.parser_for : Format -> (U64 -> Try({ value : shape, rest : U64 }, err))]
parser_of = |_| {
    Shape : shape
    Shape.parser_for(Format.Default)
}

encoder_of : shape -> (shape, U64 -> Try(U64, err))
    where [shape.encoder_for : Format -> (shape, U64 -> Try(U64, err))]
encoder_of = |_| {
    Shape : shape
    Shape.encoder_for(Format.Default)
}

# Top-level functions, whose value compile-time evaluation stores.
stored_parse : U64 -> Try({ value : Pair, rest : U64 }, [Negative])
stored_parse = parser_of((0, 0))

stored_encode : U64, U64 -> Try(U64, WideEnc)
stored_encode = encoder_of(0)

# Constants holding codecs, which a later compilation reads from the checked
# module whichever strategy evaluated it.
Config : {
    label : Str,
    parse : U64 -> Try({ value : Pair, rest : U64 }, [Negative]),
    encode : U64, U64 -> Try(U64, WideEnc),
}

closed_config : Config
closed_config = { label: "closed", parse: parser_of((0, 0)), encode: encoder_of(0) }

open_config : Config
open_config = { label: "open", parse: any_parser({}), encode: any_encoder({}) }

parse_or_reset : (U64 -> Try({ value : Pair, rest : U64 }, [Negative])), U64 -> Try({ value : Pair, rest : U64 }, Wide)
parse_or_reset = |parse, state| {
    if state == 100 {
        Err(Reset("parse reset: a string long enough to live on the heap"))
    } else if state == 200 {
        Err(Skipped(state))
    } else {
        parsed = parse(state)?
        Ok(parsed)
    }
}

encode_or_reset : (U64, U64 -> Try(U64, WideEnc)), U64, U64 -> Try(U64, WideEnc)
encode_or_reset = |encode, value, state| {
    if state == 100 {
        Err(Reset("encode reset: a string long enough to live on the heap"))
    } else if state == 200 {
        Err(Skipped(state))
    } else {
        encode(value, state)
    }
}

show_parse : Try({ value : Pair, rest : U64 }, Wide) -> Str
show_parse = |result| {
    match result {
        Ok(parsed) => "ok ${parsed.value.0.to_str()} ${parsed.value.1.to_str()} ${parsed.rest.to_str()}"
        Err(Negative) => "negative"
        Err(Reset(message)) => message
        Err(Skipped(state)) => "skipped ${state.to_str()}"
    }
}

show_encode : Try(U64, WideEnc) -> Str
show_encode = |result| {
    match result {
        Ok(state) => "ok ${state.to_str()}"
        Err(Zero) => "zero"
        Err(Reset(message)) => message
        Err(Skipped(state)) => "skipped ${state.to_str()}"
    }
}

report! = |label, parse, encode| {
    Stdout.line!(label)
    Stdout.line!(show_parse(parse_or_reset(parse, 1)))
    Stdout.line!(show_parse(parse_or_reset(parse, 7)))
    Stdout.line!(show_parse(parse_or_reset(parse, 6)))
    Stdout.line!(show_parse(parse_or_reset(parse, 100)))
    Stdout.line!(show_parse(parse_or_reset(parse, 200)))
    Stdout.line!(show_encode(encode_or_reset(encode, 3, 4)))
    Stdout.line!(show_encode(encode_or_reset(encode, 0, 4)))
    Stdout.line!(show_encode(encode_or_reset(encode, 3, 100)))
    Stdout.line!(show_encode(encode_or_reset(encode, 3, 200)))
}

main! = || {
    report!("stored", stored_parse, stored_encode)
    report!(closed_config.label, closed_config.parse, closed_config.encode)
    report!(open_config.label, open_config.parse, open_config.encode)
}
