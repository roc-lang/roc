app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Every method of this format is free to pick its error type, so the derived
# tuple parser's error type is a bare variable.
Format := [Default].{
    invalid_value : Format, U64 -> _invalid
    invalid_value = |_, _| crash "the tuple parser never reports an invalid value"

    parse_tuple_start : Format, U64, U64 -> Try(U64, _start)
    parse_tuple_start = |_, state, _| Ok(state)

    parse_tuple_next : Format, U64, U64, U64 -> Try(U64, _next)
    parse_tuple_next = |_, state, _, _| Ok(state)

    parse_tuple_end : Format, U64, U64 -> Try(U64, _end)
    parse_tuple_end = |_, state, _| Ok(state)

    parse_u64 : Format, U64 -> Try({ value : U64, rest : U64 }, _u64)
    parse_u64 = |_, state| Ok({ value: state, rest: state + 1 })
}

parse : U64 -> Try({ value : a, rest : U64 }, err)
    where [a.parser_for : Format -> (U64 -> Try({ value : a, rest : U64 }, err))]
parse = |state| {
    Shape : a
    parse_shape = Shape.parser_for(Format.Default)
    parse_shape(state)
}

main! = || {
    fallback = { value: (0.U64, 0.U64), rest: 0.U64 }
    parsed = parse(5) ?? fallback
    Stdout.line!(parsed.value.0.to_str())
    Stdout.line!(parsed.value.1.to_str())
}
