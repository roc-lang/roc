app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A format's error type need not be a tag union. This one fails with a record,
# which the derived record encoder propagates to its caller.
Format := [Default].{
    rename_field : Format, Str -> Str
    rename_field = |_, name| name

    encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)
    encode_record = |state, _, write_fields| {
        write_fields(state, |field_state, _, write_value| write_value(field_state))
    }

    encode_u64 : U64, U64 -> Try(U64, { code : U64 })
    encode_u64 = |value, state| if value == 0 { Err({ code: 7 }) } else { Ok(state + value) }
}

encode : value -> Try(U64, err)
    where [value.encoder_for : Format -> (value, U64 -> Try(U64, err))]
encode = |value| {
    Shape : value
    encode_value = Shape.encoder_for(Format.Default)
    encode_value(value, 0)
}

show : Try(U64, { code : U64 }) -> Str
show = |result| {
    match result {
        Ok(state) => state.to_str()
        Err(error) => error.code.to_str()
    }
}

main! = || {
    Stdout.line!(show(encode({ n: 0.U64 })))
    Stdout.line!(show(encode({ n: 3.U64 })))
}
