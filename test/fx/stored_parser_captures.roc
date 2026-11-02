app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A format that reads no field name: a record ends at its first field, and a
# tuple parses nothing, so a stored parser's renamed names can go unused.
Format := [Plain, Other].{
    rename_field : Format, Str -> Str
    rename_field = |_, name| name

    invalid_value : Format, U64 -> [FormatError]
    invalid_value = |_, _| FormatError

    parse_u64 : Format, U64 -> Try({ value : U64, rest : U64 }, [FormatError])
    parse_u64 = |_, state| Ok({ value: state, rest: state + 1 })

    parse_tuple_start : Format, U64, U64 -> Try(U64, [FormatError])
    parse_tuple_start = |_, _, _| Err(FormatError)

    parse_tuple_next : Format, U64, U64, U64 -> Try(U64, [FormatError])
    parse_tuple_next = |_, state, _, _| Ok(state)

    parse_tuple_end : Format, U64, U64 -> Try(U64, [FormatError])
    parse_tuple_end = |_, state, _| Ok(state)

    parse_record_start : Format, U64 -> Try([Counted({ len : U64, rest : U64 }), Uncounted(U64)], [FormatError])
    parse_record_start = |_, state| Ok(Uncounted(state))

    parse_record_field : Format,
    Encoding.FieldName.FieldNames(_shape),
    U64 -> Try(
        [
            Field({ field : Encoding.FieldName(_shape), rest : U64 }),
            TryField({ name : Str, rest : U64 }),
            TryFieldCaseless({ name : Str, rest : U64 }),
            Continue(U64),
            Done(U64),
        ],
        [FormatError],
    )
    parse_record_field = |_, _, state| Ok(Done(state + 1))

    parse_record_after_field : Format, U64 -> Try([Continue(U64), Done(U64)], [FormatError])
    parse_record_after_field = |_, state| Ok(Continue(state))

    skip_record_field : Format, U64 -> Try(U64, [FormatError])
    skip_record_field = |_, state| Ok(state)
}

# The format reaches the derived parser through a binder of this function,
# which no restore has.
parser_with : Format, shape -> (U64 -> Try({ value : shape, rest : U64 }, [FormatError]))
    where [shape.parser_for : Format -> (U64 -> Try({ value : shape, rest : U64 }, [FormatError]))]
parser_with = |format, _| {
    Shape : shape
    Shape.parser_for(format)
}

Named : { label : Try(U64, [Missing]) }

Rejected : (Named, U64)

stored_empty : U64 -> Try({ value : {}, rest : U64 }, [FormatError])
stored_empty = parser_with(Format.Other, {})

stored_named : U64 -> Try({ value : Named, rest : U64 }, [FormatError])
stored_named = parser_with(Format.Other, { label: Err(Missing) })

stored_rejected : U64 -> Try({ value : Rejected, rest : U64 }, [FormatError])
stored_rejected = parser_with(Format.Other, ({ label: Err(Missing) }, 0))

# Compile-time evaluation restores this parser and stores it again.
held : { named : (U64 -> Try({ value : Named, rest : U64 }, [FormatError])) }
held = { named: stored_named }

outer : { inner : { named : (U64 -> Try({ value : Named, rest : U64 }, [FormatError])) } }
outer = { inner: held }

show_named : Try({ value : Named, rest : U64 }, [FormatError]) -> Str
show_named = |result| {
    match result {
        Ok(parsed) => {
            label = match parsed.value.label {
                Ok(value) => value.to_str()
                Err(Missing) => "missing"
            }
            "${label} ${parsed.rest.to_str()}"
        }
        Err(FormatError) => "format error"
    }
}

run_empty : U64 -> Str
run_empty = |state| {
    match stored_empty(state) {
        Ok(parsed) => parsed.rest.to_str()
        Err(FormatError) => "format error"
    }
}

run_named : U64 -> Str
run_named = |state| show_named(stored_named(state))

run_rejected : U64 -> Str
run_rejected = |state| {
    match stored_rejected(state) {
        Ok(parsed) => parsed.value.1.to_str()
        Err(FormatError) => "format error"
    }
}

run_held : U64 -> Str
run_held = |state| show_named((outer.inner.named)(state))

main! = || {
    Stdout.line!(run_empty(4))
    Stdout.line!(run_named(4))
    Stdout.line!(run_rejected(4))
    Stdout.line!(run_held(5))
}
