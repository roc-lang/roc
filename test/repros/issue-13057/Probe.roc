Probe :: [].{}

# https://github.com/roc-lang/roc/issues/13057
# Two physically distinct signed-zero keys are one key under numeric equality,
# so the derived parser keeps the value given last. Bitwise equality keeps both.
Error : [InvalidValue]
State : List(U64)

Format := [Default].{
    invalid_value : Format, State -> Error
    invalid_value = |_, _| InvalidValue

    parse_dict_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], Error)
    parse_dict_start = |_, state| Ok(Counted({ len: 2, rest: state }))

    parse_dict_next : Format, State -> Try([Entry(State), Done(State)], Error)
    parse_dict_next = |_, _| Err(InvalidValue)

    parse_dict_after_key : Format, State -> Try(State, Error)
    parse_dict_after_key = |_, state| Ok(state)

    parse_dict_after_entry : Format, State -> Try([Continue(State), Done(State)], Error)
    parse_dict_after_entry = |_, _| Err(InvalidValue)

    parse_key_start : Format, State -> Try(State, Error)
    parse_key_start = |_, state| Ok(state)

    parse_u64 : Format, State -> Try({ value : U64, rest : State }, Error)
    parse_u64 = |_, state| match state {
        [value, .. as rest] => Ok({ value, rest })
        [] => Err(InvalidValue)
    }

    parse_f64 : Format, State -> Try({ value : F64, rest : State }, Error)
    parse_f64 = |format, state| {
        parsed = Format.parse_u64(format, state)?
        Ok({ value: F64.from_bits(parsed.value), rest: parsed.rest })
    }

    parse_key_f64 : Format, State -> Try({ value : F64, rest : State }, Error)
    parse_key_f64 = |format, state| Format.parse_f64(format, state)
}

Bits :: F64.{
    parser_for : _
    is_eq : Bits, Bits -> Bool
    is_eq = |Bits.(a), Bits.(b)| a.to_bits() == b.to_bits()
    to_hash : Bits, Hasher -> Hasher
    to_hash = |Bits.(value), hasher| value.to_bits().to_hash(hasher)
}

decode : State -> Try(a, Error)
    where [a.parser_for : Format -> (State -> Try({ value : a, rest : State }, Error))]
decode = |state| {
    Value : a
    parsed = Value.parser_for(Format.Default)(state)?
    if parsed.rest.is_empty() Ok(parsed.value) else Err(InvalidValue)
}

# Direct Dict insertion uses numeric equality for the two zero signs.
expect {
    positive = 0.0.F64
    negative = F64.from_bits(0x8000000000000000)
    values = Dict.from_list([(positive, 1.U64), (negative, 2)])
    positive == negative and values.len() == 1 and values.get(positive) == Ok(2)
}

# The same physical keys are valid when the key type uses bitwise equality.
expect {
    result : Try(Dict(Bits, U64), Error)
    result = decode([0, 1, 0x8000000000000000, 2])
    match result {
        Ok(values) =>
            values.len() == 2
            and values.get(Bits.(0.0)) == Ok(1)
            and values.get(Bits.(F64.from_bits(0x8000000000000000))) == Ok(2)
        Err(_) => False
    }
}

# Numeric equality makes the two zeros one key, which keeps the value given last.
expect {
    result : Try(Dict(F64, U64), Error)
    result = decode([0, 1, 0x8000000000000000, 2])
    match result {
        Ok(values) => values.len() == 1 and values.get(0.0) == Ok(2)
        Err(_) => False
    }
}
