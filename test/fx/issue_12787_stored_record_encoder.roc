app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A format whose record encoder renames its fields, so the stored encoder holds
# those names, and whose field encoder fails with a row every use may widen.
Format := [Default].{
    rename_field : Format, Str -> Str
    rename_field = |_, name| name

    encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)
    encode_record = |state, _, write_fields| {
        write_fields(state, |field_state, _, write_value| write_value(field_state))
    }

    encode_u64 : U64, U64 -> Try(U64, [Zero])
    encode_u64 = |value, state| if value == 0 { Err(Zero) } else { Ok(state + value) }
}

Wide : [Zero, Reset(Str)]

encoder_of : shape -> (shape, U64 -> Try(U64, err))
    where [shape.encoder_for : Format -> (shape, U64 -> Try(U64, err))]
encoder_of = |_| {
    Shape : shape
    Shape.encoder_for(Format.Default)
}

stored_encode : { n : U64 }, U64 -> Try(U64, Wide)
stored_encode = encoder_of({ n: 0 })

encode_or_reset : { n : U64 }, U64 -> Try(U64, Wide)
encode_or_reset = |value, state| {
    if state == 100 {
        Err(Reset("reset: a string long enough to live on the heap"))
    } else {
        stored_encode(value, state)
    }
}

show : Try(U64, Wide) -> Str
show = |result| {
    match result {
        Ok(state) => "ok ${state.to_str()}"
        Err(Zero) => "zero"
        Err(Reset(message)) => message
    }
}

main! = || {
    Stdout.line!(show(encode_or_reset({ n: 3 }, 4)))
    Stdout.line!(show(encode_or_reset({ n: 0 }, 4)))
    Stdout.line!(show(encode_or_reset({ n: 3 }, 100)))
}
