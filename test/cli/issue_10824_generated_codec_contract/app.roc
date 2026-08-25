app [Model, main] { pf: platform "platform/main.roc" }

Fmt := [F].{
    rename_field : Fmt, Str -> Str
    rename_field = |_, name| name

    encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])) -> Try(U64, [])
    encode_record = |state, _, write_fields|
        write_fields(state, |fs, _name, write_value| write_value(fs))

    encode_u64 : U64, U64 -> Try(U64, [])
    encode_u64 = |_value, state| Ok(state)
}

Item := [Only].{
    encoder_for : Fmt -> (Item, U64 -> Try(U64, []))
    encoder_for = |_fmt| |_value, state| Ok(state)
}

Model : {
    count : U64,
    item : Item,
}

encode_value : value -> Try(U64, [])
    where [
        value.encoder_for : Fmt -> (value, U64 -> Try(U64, [])),
    ]
encode_value = |value| {
    Shape : value
    go = Shape.encoder_for(Fmt.F)
    go(value, 0)
}

main = {
    modify: |m| { ..m, count: 1 },
    encode: |m| encode_value(m),
}
