app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A format whose record encoder ignores field names, so a stored encoder's
# renamed names are unused once the format's methods are inlined.
Format := [Plain, Other].{
    rename_field : Format, Str -> Str
    rename_field = |_, name| name

    encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)
    encode_record = |state, _, write_fields| write_fields(state, |field_state, _, write_value| write_value(field_state))

    encode_u64 : U64, U64 -> Try(U64, [Zero])
    encode_u64 = |value, state| if value == 0 { Err(Zero) } else { Ok(state + value) }
}

# The format reaches the derived encoder through a binder of this function,
# which no restore has.
encoder_with : Format, shape -> (shape, U64 -> Try(U64, err))
    where [shape.encoder_for : Format -> (shape, U64 -> Try(U64, err))]
encoder_with = |format, _| {
    Shape : shape
    Shape.encoder_for(format)
}

Pair : { first : U64, second : U64 }

stored_empty : {}, U64 -> Try(U64, [Zero])
stored_empty = encoder_with(Format.Other, {})

stored_pair : Pair, U64 -> Try(U64, [Zero])
stored_pair = encoder_with(Format.Other, { first: 0, second: 0 })

# Compile-time evaluation restores these two encoders and stores them again.
held : { empty : ({}, U64 -> Try(U64, [Zero])), pair : (Pair, U64 -> Try(U64, [Zero])) }
held = { empty: stored_empty, pair: stored_pair }

outer : { inner : { empty : ({}, U64 -> Try(U64, [Zero])), pair : (Pair, U64 -> Try(U64, [Zero])) } }
outer = { inner: held }

show : Try(U64, [Zero]) -> Str
show = |result| {
    match result {
        Ok(state) => state.to_str()
        Err(Zero) => "zero"
    }
}

run_empty : U64 -> Str
run_empty = |state| show(stored_empty({}, state))

run_pair : U64, U64 -> Str
run_pair = |first, state| show(stored_pair({ first, second: 2 }, state))

run_held_empty : U64 -> Str
run_held_empty = |state| show((outer.inner.empty)({}, state))

run_held_pair : U64, U64 -> Str
run_held_pair = |first, state| show((outer.inner.pair)({ first, second: 2 }, state))

main! = || {
    Stdout.line!(run_empty(4))
    Stdout.line!(run_pair(3, 4))
    Stdout.line!(run_pair(0, 4))
    Stdout.line!(run_held_empty(5))
    Stdout.line!(run_held_pair(3, 5))
    Stdout.line!(run_held_pair(0, 5))
}
