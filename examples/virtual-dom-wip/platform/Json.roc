module [
    Json,
    json,
    json_with_options,
]

import Encode exposing [append_with]

## An opaque type with the `EncoderFormatting` and
## `DecoderFormatting` abilities.
Json := {}
    implements [
        EncoderFormatting {
            u8: encode_u8,
            u16: encode_u16,
            u32: encode_u32,
            u64: encode_u64,
            u128: encode_u128,
            i8: encode_i8,
            i16: encode_i16,
            i32: encode_i32,
            i64: encode_i64,
            i128: encode_i128,
            f32: encode_f32,
            f64: encode_f64,
            dec: encode_dec,
            bool: encode_bool,
            string: encode_string,
            list: encode_list,
            record: encode_record,
            tuple: encode_tuple,
            tag: encode_tag,
        },
        DecoderFormatting {
            u8: decode_u8,
            u16: decode_u16,
            u32: decode_u32,
            u64: decode_u64,
            u128: decode_u128,
            i8: decode_i8,
            i16: decode_i16,
            i32: decode_i32,
            i64: decode_i64,
            i128: decode_i128,
            f32: decode_f32,
            f64: decode_f64,
            dec: decode_dec,
            bool: decode_bool,
            string: decode_string,
            list: decode_list,
            record: decode_record,
            tuple: decode_tuple,
        },
    ]

## Returns a JSON `Encoder` and `Decoder`
json = @Json({})

## Returns a JSON `Encoder` and `Decoder` with configuration options
json_with_options = \{} ->
    @Json({})

# TODO encode as JSON numbers as base 10 decimal digits
# e.g. the REPL `Num.toStr 12e42f64` gives
# "12000000000000000000000000000000000000000000" : Str
# which should be encoded as "12e42" : Str
num_to_bytes = \n ->
    n |> Num.to_str |> Str.to_utf8

encode_u8 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_u16 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_u32 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_u64 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_u128 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_i8 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_i16 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_i32 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_i64 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_i128 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_f32 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_f64 = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_dec = \n ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, num_to_bytes(n)))

encode_bool = \b ->
    Encode.custom(\bytes, @Json({}) ->
        if b then
            List.concat(bytes, Str.to_utf8("true"))
        else
            List.concat(bytes, Str.to_utf8("false")))

encode_string = \str ->
    Encode.custom(\bytes, @Json({}) ->
        List.concat(bytes, encode_str_bytes(str)))

# TODO add support for unicode escapes (including 2,3,4 byte code points)
# these should be encoded using a 12-byte sequence encoding the UTF-16 surrogate
# pair. For example a string containing only G clef character U+1D11E is
# represented as "\\uD834\\uDD1E" (note "\\" here is a single reverse solidus)
encode_str_bytes = \str ->
    bytes = Str.to_utf8(str)

    initial_state = { byte_pos: 0, status: NoEscapesFound }

    first_pass_state =
        List.walk_until(bytes, initial_state, \{ byte_pos, status }, b ->
            when b is
                0x22 -> Break({ byte_pos, status: FoundEscape }) # U+0022 Quotation mark
                0x5c -> Break({ byte_pos, status: FoundEscape }) # U+005c Reverse solidus
                0x2f -> Break({ byte_pos, status: FoundEscape }) # U+002f Solidus
                0x08 -> Break({ byte_pos, status: FoundEscape }) # U+0008 Backspace
                0x0c -> Break({ byte_pos, status: FoundEscape }) # U+000c Form feed
                0x0a -> Break({ byte_pos, status: FoundEscape }) # U+000a Line feed
                0x0d -> Break({ byte_pos, status: FoundEscape }) # U+000d Carriage return
                0x09 -> Break({ byte_pos, status: FoundEscape }) # U+0009 Tab
                _ -> Continue({ byte_pos: byte_pos + 1, status }))

    when first_pass_state.status is
        NoEscapesFound ->
            (List.len(bytes))
            + 2
            |> List.with_capacity
            |> List.concat(['"'])
            |> List.concat(bytes)
            |> List.concat(['"'])

        FoundEscape ->
            { before: bytes_before_escape, others: bytes_with_escapes } =
                List.split_at(bytes, first_pass_state.byte_pos)

            # Reserve List with 120% capacity for escaped bytes to reduce
            # allocations, include starting quote, and bytes up to first escape
            initial =
                List.len(bytes)
                |> Num.mul(120)
                |> Num.div_ceil(100)
                |> List.with_capacity
                |> List.concat(['"'])
                |> List.concat(bytes_before_escape)

            # Walk the remaining bytes and include escape '\' as required
            # add closing quote
            List.walk(bytes_with_escapes, initial, \encoded_bytes, byte ->
                List.concat(encoded_bytes, escaped_byte_to_json(byte)))
            |> List.concat(['"'])

# Prepend an "\" escape byte
escaped_byte_to_json : U8 -> List U8
escaped_byte_to_json = \b ->
    when b is
        0x22 -> [0x5c, 0x22] # U+0022 Quotation mark
        0x5c -> [0x5c, 0x5c] # U+005c Reverse solidus
        0x2f -> [0x5c, 0x2f] # U+002f Solidus
        0x08 -> [0x5c, 'b'] # U+0008 Backspace
        0x0c -> [0x5c, 'f'] # U+000c Form feed
        0x0a -> [0x5c, 'n'] # U+000a Line feed
        0x0d -> [0x5c, 'r'] # U+000d Carriage return
        0x09 -> [0x5c, 'r'] # U+0009 Tab
        _ -> [b]

encode_list = \lst, encode_elem ->
    Encode.custom(\bytes, @Json({}) ->
        write_list = \{ buffer, elems_left }, elem ->
            before_buffer_len = buffer |> List.len

            buffer_with_elem = append_with(buffer, encode_elem(elem), @Json({}))
            # If our encoder returned [] we just skip the elem
            if buffer_with_elem |> List.len == before_buffer_len then
                { buffer: buffer_with_elem, elems_left: elems_left - 1 }
            else
                buffer_with_suffix =
                    if elems_left > 1 then
                        List.append(buffer_with_elem, Num.to_u8(','))
                    else
                        buffer_with_elem

                { buffer: buffer_with_suffix, elems_left: elems_left - 1 }

        head = List.append(bytes, Num.to_u8('['))
        { buffer: with_list } = List.walk(lst, { buffer: head, elems_left: List.len(lst) }, write_list)

        List.append(with_list, Num.to_u8(']')))

encode_record = \fields ->
    Encode.custom(\bytes, @Json({}) ->
        write_record = \{ buffer, fields_left }, { key, value } ->

            field_value = [] |> append_with(value, json)
            # If our encoder returned [] we just skip the field
            if field_value == [] then
                { buffer, fields_left: fields_left - 1 }
            else
                field_name = key
                buffer_with_key_value =
                    List.append(buffer, Num.to_u8('"'))
                    |> List.concat(Str.to_utf8(field_name))
                    |> List.append(Num.to_u8('"'))
                    |> List.append(Num.to_u8(':')) # Note we need to encode using the json config here
                    |> List.concat(field_value)

                buffer_with_suffix =
                    if fields_left > 1 then
                        List.append(buffer_with_key_value, Num.to_u8(','))
                    else
                        buffer_with_key_value

                { buffer: buffer_with_suffix, fields_left: fields_left - 1 }

        bytes_head = List.append(bytes, Num.to_u8('{'))
        { buffer: bytes_with_record } = List.walk(fields, { buffer: bytes_head, fields_left: List.len(fields) }, write_record)

        List.append(bytes_with_record, Num.to_u8('}')))

encode_tuple = \elems ->
    Encode.custom(\bytes, @Json({}) ->
        write_tuple = \{ buffer, elems_left }, elem_encoder ->
            before_buffer_len = buffer |> List.len

            buffer_with_elem = append_with(buffer, elem_encoder, @Json({}))

            # If our encoder returned [] we just skip the elem
            if buffer_with_elem |> List.len == before_buffer_len then
                { buffer: buffer_with_elem, elems_left: elems_left - 1 }
            else
                buffer_with_suffix =
                    if elems_left > 1 then
                        List.append(buffer_with_elem, Num.to_u8(','))
                    else
                        buffer_with_elem

                { buffer: buffer_with_suffix, elems_left: elems_left - 1 }

        bytes_head = List.append(bytes, Num.to_u8('['))
        { buffer: bytes_with_record } = List.walk(elems, { buffer: bytes_head, elems_left: List.len(elems) }, write_tuple)

        List.append(bytes_with_record, Num.to_u8(']')))
encode_tag = \name, payload ->
    Encode.custom(\bytes, @Json({}) ->
        # Idea: encode `A v1 v2` as `{"A": [v1, v2]}`
        write_payload = \{ buffer, items_left }, encoder ->
            buffer_with_value = append_with(buffer, encoder, @Json({}))
            buffer_with_suffix =
                if items_left > 1 then
                    List.append(buffer_with_value, Num.to_u8(','))
                else
                    buffer_with_value

            { buffer: buffer_with_suffix, items_left: items_left - 1 }

        bytes_head =
            List.append(bytes, Num.to_u8('{'))
            |> List.append(Num.to_u8('"'))
            |> List.concat(Str.to_utf8(name))
            |> List.append(Num.to_u8('"'))
            |> List.append(Num.to_u8(':'))
            |> List.append(Num.to_u8('['))

        { buffer: bytes_with_payload } = List.walk(payload, { buffer: bytes_head, items_left: List.len(payload) }, write_payload)

        List.append(bytes_with_payload, Num.to_u8(']'))
        |> List.append(Num.to_u8('}')))

decode_u8 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_u8)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of U8
expect
    actual = Str.to_utf8("255") |> Decode.from_bytes(json)
    actual == Ok(255u8)

decode_u16 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_u16)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of U16
expect
    actual = Str.to_utf8("65535") |> Decode.from_bytes(json)
    actual == Ok(65_535u16)

decode_u32 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_u32)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of U32
expect
    actual = Str.to_utf8("4000000000") |> Decode.from_bytes(json)
    actual == Ok(4_000_000_000u32)

decode_u64 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_u64)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of U64
expect
    actual = Str.to_utf8("18446744073709551614") |> Decode.from_bytes(json)
    actual == Ok(18_446_744_073_709_551_614u64)

decode_u128 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_u128)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of U128
expect
    actual = Str.to_utf8("1234567") |> Decode.from_bytes_partial(json)
    actual.result == Ok(1234567u128)

# TODO should we support decoding bigints, note that valid json is only a
# double precision float-64
# expect
#     actual = Str.toUtf8 "340282366920938463463374607431768211455" |> Decode.fromBytesPartial json
#     actual.result == Ok 340_282_366_920_938_463_463_374_607_431_768_211_455u128

decode_i8 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_i8)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of I8
expect
    actual = Str.to_utf8("-125") |> Decode.from_bytes_partial(json)
    actual.result == Ok(-125i8)

decode_i16 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_i16)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of I16
expect
    actual = Str.to_utf8("-32768") |> Decode.from_bytes_partial(json)
    actual.result == Ok(-32_768i16)

decode_i32 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_i32)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of I32
expect
    actual = Str.to_utf8("-2147483648") |> Decode.from_bytes_partial(json)
    actual.result == Ok(-2_147_483_648i32)

decode_i64 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_i64)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of I64
expect
    actual = Str.to_utf8("-9223372036854775808") |> Decode.from_bytes_partial(json)
    actual.result == Ok(-9_223_372_036_854_775_808i64)

decode_i128 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_i128)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of I128
# expect
#     actual = Str.toUtf8 "-170141183460469231731687303715884105728" |> Decode.fromBytesPartial json
#     actual.result == Ok -170_141_183_460_469_231_731_687_303_715_884_105_728i128

decode_f32 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_f32)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of F32
expect
    actual : DecodeResult F32
    actual = Str.to_utf8("12.34e-5") |> Decode.from_bytes_partial(json)
    num_str = actual.result |> Result.map(Num.to_str)

    Result.with_default(num_str, "") == "0.00012339999375399202"

decode_f64 = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_f64)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of F64
expect
    actual : DecodeResult F64
    actual = Str.to_utf8("12.34e-5") |> Decode.from_bytes_partial(json)
    num_str = actual.result |> Result.map(Num.to_str)

    Result.with_default(num_str, "") == "0.0001234"

decode_dec = Decode.custom(\bytes, @Json({}) ->
    { taken, rest } = take_json_number(bytes)

    result =
        taken
        |> Str.from_utf8
        |> Result.try(Str.to_dec)
        |> Result.map_err(\_ -> TooShort)

    { result, rest })

# Test decode of Dec
expect
    actual : DecodeResult Dec
    actual = Str.to_utf8("12.0034") |> Decode.from_bytes_partial(json)

    actual.result == Ok(12.0034dec)

decode_bool = Decode.custom(\bytes, @Json({}) ->
    when bytes is
        ['f', 'a', 'l', 's', 'e', ..] -> { result: Ok(Bool.false), rest: List.drop_first(bytes, 5) }
        ['t', 'r', 'u', 'e', ..] -> { result: Ok(Bool.true), rest: List.drop_first(bytes, 4) }
        _ -> { result: Err(TooShort), rest: bytes })

# Test decode of Bool
expect
    actual = "true\n" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Ok(Bool.true)
    actual.result == expected

# Test decode of Bool
expect
    actual = "false ]\n" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Ok(Bool.false)
    actual.result == expected

decode_tuple = \initial_state, step_elem, finalizer ->
    Decode.custom(\initial_bytes, @Json({}) ->
        # NB: the stepper function must be passed explicitly until #2894 is resolved.
        decode_elems = \stepper, state, index, bytes ->
            (
                when stepper(state, index) is
                    TooLong ->
                        bytes
                        |> anything
                        |> try_decode(\{ rest: before_comma_or_break } ->
                            { result: Ok(state), rest: before_comma_or_break })

                    Next(decoder) ->
                        Decode.decode_with(bytes, decoder, json)
            )
            |> try_decode(\{ val: new_state, rest: before_comma_or_break } ->
                { result: comma_result, rest: next_bytes } = comma(before_comma_or_break)

                when comma_result is
                    Ok({}) -> decode_elems(step_elem, new_state, (index + 1), next_bytes)
                    Err(_) -> { result: Ok(new_state), rest: next_bytes })

        initial_bytes
        |> open_bracket
        |> try_decode(\{ rest: after_bracket_bytes } ->
            decode_elems(step_elem, initial_state, 0, after_bracket_bytes)
            |> try_decode(\{ val: end_state_result, rest: before_closing_bracket_bytes } ->
                before_closing_bracket_bytes
                |> closing_bracket
                |> try_decode(\{ rest: after_tuple_bytes } ->
                    when finalizer(end_state_result) is
                        Ok(val) -> { result: Ok(val), rest: after_tuple_bytes }
                        Err(e) -> { result: Err(e), rest: after_tuple_bytes }))))

# Test decode of tuple
expect
    input = Str.to_utf8("[\"The Answer is\",42]")
    actual = Decode.from_bytes_partial(input, json)

    actual.result == Ok(("The Answer is", 42))

parse_exact_char : List U8, U8 -> DecodeResult {}
parse_exact_char = \bytes, char ->
    when List.get(bytes, 0) is
        Ok(c) ->
            if
                c == char
            then
                { result: Ok({}), rest: (List.split_at(bytes, 1)).others }
            else
                { result: Err(TooShort), rest: bytes }

        Err(_) -> { result: Err(TooShort), rest: bytes }

open_bracket : List U8 -> DecodeResult {}
open_bracket = \bytes -> parse_exact_char(bytes, '[')

closing_bracket : List U8 -> DecodeResult {}
closing_bracket = \bytes -> parse_exact_char(bytes, ']')

anything : List U8 -> DecodeResult {}
anything = \bytes -> { result: Err(TooShort), rest: bytes }

comma : List U8 -> DecodeResult {}
comma = \bytes -> parse_exact_char(bytes, ',')

try_decode : DecodeResult a, ({ val : a, rest : List U8 } -> DecodeResult b) -> DecodeResult b
try_decode = \{ result, rest }, mapper ->
    when result is
        Ok(val) -> mapper({ val, rest })
        Err(e) -> { result: Err(e), rest }

# JSON NUMBER PRIMITIVE --------------------------------------------------------

# Takes the bytes for a valid Json number primitive into a RocStr
#
# Note that this does not handle leading whitespace, any whitespace must be
# handled in json list or record decoding.
#
# |> List.dropIf \b -> b == '+'
# TODO ^^ not needed if roc supports "1e+2", this supports
# "+" which is permitted in Json numbers
#
# |> List.map \b -> if b == 'E' then 'e' else b
# TODO ^^ not needed if roc supports "1E2", this supports
# "E" which is permitted in Json numbers
take_json_number : List U8 -> { taken : List U8, rest : List U8 }
take_json_number = \bytes ->
    when List.walk_until(bytes, Start, number_help) is
        Finish(n) | Zero(n) | Integer(n) | FractionB(n) | ExponentC(n) ->
            taken =
                bytes
                |> List.sublist({ start: 0, len: n })
                |> List.drop_if(\b -> b == '+')
                |> List.map(\b -> if b == 'E' then 'e' else b)

            { taken, rest: List.drop_first(bytes, n) }

        _ ->
            { taken: [], rest: bytes }

number_help : NumberState, U8 -> [Continue NumberState, Break NumberState]
number_help = \state, byte ->
    when (state, byte) is
        (Start, b) if b == '0' -> Continue(Zero(1))
        (Start, b) if b == '-' -> Continue(Minus(1))
        (Start, b) if is_digit1to9(b) -> Continue(Integer(1))
        (Minus(n), b) if b == '0' -> Continue(Zero((n + 1)))
        (Minus(n), b) if is_digit1to9(b) -> Continue(Integer((n + 1)))
        (Zero(n), b) if b == '.' -> Continue(FractionA((n + 1)))
        (Zero(n), b) if is_valid_end(b) -> Break(Finish(n))
        (Integer(n), b) if is_digit0to9(b) && n <= max_bytes -> Continue(Integer((n + 1)))
        (Integer(n), b) if b == '.' && n < max_bytes -> Continue(FractionA((n + 1)))
        (Integer(n), b) if is_valid_end(b) && n <= max_bytes -> Break(Finish(n))
        (FractionA(n), b) if is_digit0to9(b) && n <= max_bytes -> Continue(FractionB((n + 1)))
        (FractionB(n), b) if is_digit0to9(b) && n <= max_bytes -> Continue(FractionB((n + 1)))
        (FractionB(n), b) if b == 'e' || b == 'E' && n <= max_bytes -> Continue(ExponentA((n + 1)))
        (FractionB(n), b) if is_valid_end(b) && n <= max_bytes -> Break(Finish(n))
        (ExponentA(n), b) if b == '-' || b == '+' && n <= max_bytes -> Continue(ExponentB((n + 1)))
        (ExponentA(n), b) if is_digit0to9(b) && n <= max_bytes -> Continue(ExponentC((n + 1)))
        (ExponentB(n), b) if is_digit0to9(b) && n <= max_bytes -> Continue(ExponentC((n + 1)))
        (ExponentC(n), b) if is_digit0to9(b) && n <= max_bytes -> Continue(ExponentC((n + 1)))
        (ExponentC(n), b) if is_valid_end(b) && n <= max_bytes -> Break(Finish(n))
        _ -> Break(Invalid)

NumberState : [
    Start,
    Minus U64,
    Zero U64,
    Integer U64,
    FractionA U64,
    FractionB U64,
    ExponentA U64,
    ExponentB U64,
    ExponentC U64,
    Invalid,
    Finish U64,
]

# TODO confirm if we would like to be able to decode
# "340282366920938463463374607431768211455" which is MAX U128 and 39 bytes
max_bytes : U64
max_bytes = 21 # Max bytes in a double precision float

is_digit0to9 : U8 -> Bool
is_digit0to9 = \b -> b >= '0' && b <= '9'

is_digit1to9 : U8 -> Bool
is_digit1to9 = \b -> b >= '1' && b <= '9'

is_valid_end : U8 -> Bool
is_valid_end = \b ->
    when b is
        ']' | ',' | ' ' | '\n' | '\r' | '\t' | '}' -> Bool.true
        _ -> Bool.false

expect
    actual = "0.0" |> Str.to_utf8 |> Decode.from_bytes(json)
    expected = Ok(0.0dec)
    actual == expected

expect
    actual = "0" |> Str.to_utf8 |> Decode.from_bytes(json)
    expected = Ok(0u8)
    actual == expected

expect
    actual = "1 " |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = { result: Ok(1dec), rest: [' '] }
    actual == expected

expect
    actual = "2]" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = { result: Ok(2u64), rest: [']'] }
    actual == expected

expect
    actual = "30,\n" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = { result: Ok(30i64), rest: [',', '\n'] }
    actual == expected

expect
    actual : DecodeResult U16
    actual = "+1" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = { result: Err(TooShort), rest: ['+', '1'] }
    actual == expected

expect
    actual : DecodeResult U16
    actual = ".0" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = { result: Err(TooShort), rest: ['.', '0'] }
    actual == expected

expect
    actual : DecodeResult U64
    actual = "-.1" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    actual.result == Err(TooShort)

expect
    actual : DecodeResult Dec
    actual = "72" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Ok(72dec)
    actual.result == expected

expect
    actual : DecodeResult Dec
    actual = "-0" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Ok(0dec)
    actual.result == expected

expect
    actual : DecodeResult Dec
    actual = "-7" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Ok(-7dec)
    actual.result == expected

expect
    actual : DecodeResult Dec
    actual = "-0\n" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = { result: Ok(0dec), rest: ['\n'] }
    actual == expected

expect
    actual : DecodeResult Dec
    actual = "123456789000 \n" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = { result: Ok(123456789000dec), rest: [' ', '\n'] }
    actual == expected

expect
    actual : DecodeResult Dec
    actual = "-12.03" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Ok(-12.03)
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "-12." |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Err(TooShort)
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "01.1" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Err(TooShort)
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = ".0" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Err(TooShort)
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "1.e1" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Err(TooShort)
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "-1.2E" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Err(TooShort)
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "0.1e+" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Err(TooShort)
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "-03" |> Str.to_utf8 |> Decode.from_bytes_partial(json)
    expected = Err(TooShort)
    actual.result == expected

# JSON STRING PRIMITIVE --------------------------------------------------------

# Decode a Json string primitive into a RocStr
#
# Note that decodeStr does not handle leading whitespace, any whitespace must be
# handled in json list or record decodin.
decode_string = Decode.custom(\bytes, @Json({}) ->
    when bytes is
        ['n', 'u', 'l', 'l', ..] ->
            { result: Ok("null"), rest: List.drop_first(bytes, 4) }

        _ ->
            { taken: str_bytes, rest } = take_json_string(bytes)

            if List.is_empty(str_bytes) then
                { result: Err(TooShort), rest: bytes }
            else
                # Remove starting and ending quotation marks, replace unicode
                # escpapes with Roc equivalent, and try to parse RocStr from
                # bytes
                result =
                    str_bytes
                    |> List.sublist({
                        start: 1,
                        len: Num.sub_saturated(List.len(str_bytes), 2),
                    })
                    |> \bytes_without_quotation_marks ->
                        replace_escaped_chars({ in_bytes: bytes_without_quotation_marks, out_bytes: [] })
                    |> .out_bytes
                    |> Str.from_utf8

                when result is
                    Ok(str) ->
                        { result: Ok(str), rest }

                    Err(_) ->
                        { result: Err(TooShort), rest: bytes })

take_json_string : List U8 -> { taken : List U8, rest : List U8 }
take_json_string = \bytes ->
    when List.walk_until(bytes, Start, string_help) is
        Finish(n) ->
            {
                taken: List.sublist(bytes, { start: 0, len: n }),
                rest: List.drop_first(bytes, n),
            }

        _ ->
            { taken: [], rest: bytes }

string_help : StringState, U8 -> [Continue StringState, Break StringState]
string_help = \state, byte ->
    when (state, byte) is
        (Start, b) if b == '"' -> Continue(Chars(1))
        (Chars(n), b) if b == '"' -> Break(Finish((n + 1)))
        (Chars(n), b) if b == '\\' -> Continue(Escaped((n + 1)))
        (Chars(n), _) -> Continue(Chars((n + 1)))
        (Escaped(n), b) if is_escaped_char(b) -> Continue(Chars((n + 1)))
        (Escaped(n), b) if b == 'u' -> Continue(UnicodeA((n + 1)))
        (UnicodeA(n), b) if is_hex(b) -> Continue(UnicodeB((n + 1)))
        (UnicodeB(n), b) if is_hex(b) -> Continue(UnicodeC((n + 1)))
        (UnicodeC(n), b) if is_hex(b) -> Continue(UnicodeD((n + 1)))
        (UnicodeD(n), b) if is_hex(b) -> Continue(Chars((n + 1)))
        _ -> Break(InvalidNumber)

StringState : [
    Start,
    Chars U64,
    Escaped U64,
    UnicodeA U64,
    UnicodeB U64,
    UnicodeC U64,
    UnicodeD U64,
    Finish U64,
    InvalidNumber,
]

is_escaped_char : U8 -> Bool
is_escaped_char = \b ->
    when b is
        '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' -> Bool.true
        _ -> Bool.false

escaped_char_from_json : U8 -> U8
escaped_char_from_json = \b ->
    when b is
        '"' -> 0x22 # U+0022 Quotation mark
        '\\' -> 0x5c # U+005c Reverse solidus
        '/' -> 0x2f # U+002f Solidus
        'b' -> 0x08 # U+0008 Backspace
        'f' -> 0x0c # U+000c Form feed
        'n' -> 0x0a # U+000a Line feed
        'r' -> 0x0d # U+000d Carriage return
        't' -> 0x09 # U+0009 Tab
        _ -> b

expect escaped_char_from_json('n') == '\n'

is_hex : U8 -> Bool
is_hex = \b ->
    (b >= '0' && b <= '9')
    || (b >= 'a' && b <= 'f')
    || (b >= 'A' && b <= 'F')

expect is_hex('0') && is_hex('f') && is_hex('F') && is_hex('A') && is_hex('9')
expect !(is_hex('g') && is_hex('x') && is_hex('u') && is_hex('\\') && is_hex('-'))

json_hex_to_decimal : U8 -> U8
json_hex_to_decimal = \b ->
    if b >= '0' && b <= '9' then
        b - '0'
    else if b >= 'a' && b <= 'f' then
        b - 'a' + 10
    else if b >= 'A' && b <= 'F' then
        b - 'A' + 10
    else
        crash("got an invalid hex char")

expect json_hex_to_decimal('0') == 0
expect json_hex_to_decimal('9') == 9
expect json_hex_to_decimal('a') == 10
expect json_hex_to_decimal('A') == 10
expect json_hex_to_decimal('f') == 15
expect json_hex_to_decimal('F') == 15

decimal_hex_to_byte : U8, U8 -> U8
decimal_hex_to_byte = \upper, lower ->
    Num.bitwise_or(Num.shift_left_by(upper, 4), lower)

expect
    actual = decimal_hex_to_byte(3, 7)
    expected = '7'
    actual == expected

expect
    actual = decimal_hex_to_byte(7, 4)
    expected = 't'
    actual == expected

hex_to_utf8 : U8, U8, U8, U8 -> List U8
hex_to_utf8 = \a, b, c, d ->
    i = json_hex_to_decimal(a)
    j = json_hex_to_decimal(b)
    k = json_hex_to_decimal(c)
    l = json_hex_to_decimal(d)

    if i == 0 && j == 0 then
        [decimal_hex_to_byte(k, l)]
    else
        [decimal_hex_to_byte(i, j), decimal_hex_to_byte(k, l)]

# Test for \u0074 == U+74 == 't' in Basic Multilingual Plane
expect
    actual = hex_to_utf8('0', '0', '7', '4')
    expected = ['t']
    actual == expected

# Test for \u0068 == U+68 == 'h' in Basic Multilingual Plane
expect
    actual = hex_to_utf8('0', '0', '6', '8')
    expected = ['h']
    actual == expected

# Test for \u2c64 == U+2C64 == 'Ɽ' in Latin Extended-C
expect
    actual = hex_to_utf8('2', 'C', '6', '4')
    expected = [44, 100]
    actual == expected

unicode_replacement = hex_to_utf8('f', 'f', 'd', 'd')

replace_escaped_chars : { in_bytes : List U8, out_bytes : List U8 } -> { in_bytes : List U8, out_bytes : List U8 }
replace_escaped_chars = \{ in_bytes, out_bytes } ->

    first_byte = List.get(in_bytes, 0)
    second_byte = List.get(in_bytes, 1)
    in_bytes_without_first_two = List.drop_first(in_bytes, 2)
    in_bytes_without_first_six = List.drop_first(in_bytes, 6)

    when Pair(first_byte, second_byte) is
        Pair(Ok(a), Ok(b)) if a == '\\' && b == 'u' ->
            # Extended json unicode escape
            when in_bytes_without_first_two is
                [c, d, e, f, ..] ->
                    utf8_bytes = hex_to_utf8(c, d, e, f)

                    replace_escaped_chars({
                        in_bytes: in_bytes_without_first_six,
                        out_bytes: List.concat(out_bytes, utf8_bytes),
                    })

                _ ->
                    # Invalid Unicode Escape
                    replace_escaped_chars({
                        in_bytes: in_bytes_without_first_two,
                        out_bytes: List.concat(out_bytes, unicode_replacement),
                    })

        Pair(Ok(a), Ok(b)) if a == '\\' && is_escaped_char(b) ->
            # Shorthand json unicode escape
            replace_escaped_chars({
                in_bytes: in_bytes_without_first_two,
                out_bytes: List.append(out_bytes, escaped_char_from_json(b)),
            })

        Pair(Ok(a), _) ->
            # Process next character
            replace_escaped_chars({
                in_bytes: List.drop_first(in_bytes, 1),
                out_bytes: List.append(out_bytes, a),
            })

        _ ->
            { in_bytes, out_bytes }

# Test replacement of both extended and shorthand unicode escapes
expect
    in_bytes = Str.to_utf8("\\\\\\u0074\\u0068\\u0065\\t\\u0071\\u0075\\u0069\\u0063\\u006b\\n")
    actual = replace_escaped_chars({ in_bytes, out_bytes: [] })
    expected = { in_bytes: [], out_bytes: ['\\', 't', 'h', 'e', '\t', 'q', 'u', 'i', 'c', 'k', '\n'] }

    actual == expected

# Test decode simple string
expect
    input = "\"hello\", " |> Str.to_utf8
    actual = Decode.from_bytes_partial(input, json)
    expected = Ok("hello")

    actual.result == expected

# Test decode string with extended and shorthand json escapes
expect
    input = "\"h\\\"\\u0065llo\\n\"]\n" |> Str.to_utf8
    actual = Decode.from_bytes_partial(input, json)
    expected = Ok("h\"ello\n")

    actual.result == expected

# Test json string decoding with escapes
expect
    input = Str.to_utf8("\"a\r\nbc\\txz\"\t\n,  ")
    actual = Decode.from_bytes_partial(input, json)
    expected = Ok("a\r\nbc\txz")

    actual.result == expected

# Test decode of a null
expect
    input = Str.to_utf8("null")
    actual = Decode.from_bytes_partial(input, json)
    expected = Ok("null")

    actual.result == expected

# JSON ARRAYS ------------------------------------------------------------------

decode_list = \elem_decoder ->
    Decode.custom(\bytes, @Json({}) ->

        decode_elems = array_elem_decoder(elem_decoder)

        result =
            when List.walk_until(bytes, BeforeOpeningBracket(0), array_opening_help) is
                AfterOpeningBracket(n) -> Ok(List.drop_first(bytes, n))
                _ -> Err(ExpectedOpeningBracket)

        when result is
            Ok(elem_bytes) -> decode_elems(elem_bytes, [])
            Err(ExpectedOpeningBracket) ->
                crash("expected opening bracket"))

array_elem_decoder = \elem_decoder ->

    decode_elems = \bytes, accum ->

        # Done't need a comma before the first element
        state =
            if List.is_empty(accum) then
                BeforeNextElement(0)
            else
                BeforeNextElemOrClosingBracket(0)

        when List.walk_until(bytes, state, array_closing_help) is
            AfterClosingBracket(n) ->
                # Eat remaining whitespace
                rest = List.drop_first(bytes, n)

                # Return List of decoded elements
                { result: Ok(accum), rest }

            BeforeNextElement(n) ->
                # Eat any whitespace before element
                elem_bytes = List.drop_first(bytes, n)

                # Decode current element
                { result, rest } = Decode.decode_with(elem_bytes, elem_decoder, json)

                when result is
                    Ok(elem) ->
                        # Accumulate decoded value and walk to next element
                        # or the end of the list
                        decode_elems(rest, List.append(accum, elem))

                    Err(_) ->
                        # Unable to decode next element
                        { result: Err(TooShort), rest }

            BeforeNextElemOrClosingBracket(_) ->
                if List.is_empty(accum) then
                    # Handle empty lists
                    { result: Ok([]), rest: bytes }
                else
                    # Expected comma or closing bracket after last element
                    { result: Err(TooShort), rest: bytes }

    decode_elems

array_opening_help : ArrayOpeningState, U8 -> [Continue ArrayOpeningState, Break ArrayOpeningState]
array_opening_help = \state, byte ->
    when (state, byte) is
        (BeforeOpeningBracket(n), b) if is_whitespace(b) -> Continue(BeforeOpeningBracket((n + 1)))
        (BeforeOpeningBracket(n), b) if b == '[' -> Continue(AfterOpeningBracket((n + 1)))
        (AfterOpeningBracket(n), b) if is_whitespace(b) -> Continue(AfterOpeningBracket((n + 1)))
        _ -> Break(state)

array_closing_help : ArrayClosingState, U8 -> [Continue ArrayClosingState, Break ArrayClosingState]
array_closing_help = \state, byte ->
    when (state, byte) is
        (BeforeNextElemOrClosingBracket(n), b) if is_whitespace(b) -> Continue(BeforeNextElemOrClosingBracket((n + 1)))
        (BeforeNextElemOrClosingBracket(n), b) if b == ',' -> Continue(BeforeNextElement((n + 1)))
        (BeforeNextElemOrClosingBracket(n), b) if b == ']' -> Continue(AfterClosingBracket((n + 1)))
        (BeforeNextElement(n), b) if is_whitespace(b) -> Continue(BeforeNextElement((n + 1)))
        (BeforeNextElement(n), b) if b == ']' -> Continue(AfterClosingBracket((n + 1)))
        (AfterClosingBracket(n), b) if is_whitespace(b) -> Continue(AfterClosingBracket((n + 1)))
        _ -> Break(state)

is_whitespace = \b ->
    when b is
        ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false

expect
    input = ['1', 'a', ' ', '\n', 0x0d, 0x09]
    actual = List.map(input, is_whitespace)
    expected = [Bool.false, Bool.false, Bool.true, Bool.true, Bool.true, Bool.true]

    actual == expected

ArrayOpeningState : [
    BeforeOpeningBracket U64,
    AfterOpeningBracket U64,
]

ArrayClosingState : [
    BeforeNextElemOrClosingBracket U64,
    BeforeNextElement U64,
    AfterClosingBracket U64,
]

# Test decoding an empty array
expect
    input = Str.to_utf8("[ ]")

    actual : DecodeResult (List U8)
    actual = Decode.from_bytes_partial(input, json)

    actual.result == Ok([])

# Test decode array of json numbers with whitespace
expect
    input = Str.to_utf8("\n[\t 1 , 2  , 3]")

    actual : DecodeResult (List U64)
    actual = Decode.from_bytes_partial(input, json)

    expected = Ok([1, 2, 3])

    actual.result == expected

# Test decode array of json strings ignoring whitespace
expect
    input = Str.to_utf8("\n\t [\n \"one\"\r , \"two\" , \n\"3\"\t]")

    actual : DecodeResult (List Str)
    actual = Decode.from_bytes_partial(input, json)
    expected = Ok(["one", "two", "3"])

    actual.result == expected

# JSON OBJECTS -----------------------------------------------------------------

decode_record = \initial_state, step_field, finalizer ->
    Decode.custom(\bytes, @Json({}) ->

        # Recursively build up record from object field:value pairs
        decode_fields = \record_state, bytes_before_field ->

            # Decode the json string field name
            { result: object_name_result, rest: bytes_after_field } =
                Decode.decode_with(bytes_before_field, decode_string, json)

            # Count the bytes until the field value
            count_bytes_before_value =
                when List.walk_until(bytes_after_field, BeforeColon(0), object_help) is
                    AfterColon(n) -> n
                    _ -> 0

            value_bytes = List.drop_first(bytes_after_field, count_bytes_before_value)

            when object_name_result is
                Err(TooShort) ->
                    # Invalid object, unable to decode field name or find colon ':'
                    # after field and before the value
                    { result: Err(TooShort), rest: bytes }

                Ok(object_name) ->
                    # Decode the json value
                    (
                        field_name = object_name

                        # Retrieve value decoder for the current field
                        when step_field(record_state, field_name) is
                            Skip ->
                                # TODO This doesn't seem right, shouldn't we eat
                                # the remaining json object value bytes if we are skipping this
                                # field?
                                { result: Ok(record_state), rest: value_bytes }

                            Keep(value_decoder) ->
                                # Decode the value using the decoder from the recordState
                                # Note we need to pass json config options recursively here
                                Decode.decode_with(value_bytes, value_decoder, @Json({}))
                    )
                    |> try_decode(\{ val: updated_record, rest: bytes_after_value } ->
                        # Check if another field or '}' for end of object
                        when List.walk_until(bytes_after_value, AfterObjectValue(0), object_help) is
                            ObjectFieldNameStart(n) ->
                                rest = List.drop_first(bytes_after_value, n)

                                # Decode the next field and value
                                decode_fields(updated_record, rest)

                            AfterClosingBrace(n) ->
                                rest = List.drop_first(bytes_after_value, n)

                                # Build final record from decoded fields and values
                                when finalizer(updated_record, json) is
                                    Ok(val) -> { result: Ok(val), rest }
                                    Err(e) -> { result: Err(e), rest }

                            _ ->
                                # Invalid object
                                { result: Err(TooShort), rest: bytes_after_value })

        count_bytes_before_first_field =
            when List.walk_until(bytes, BeforeOpeningBrace(0), object_help) is
                ObjectFieldNameStart(n) -> n
                _ -> 0

        if count_bytes_before_first_field == 0 then
            # Invalid object, expected opening brace '{' followed by a field
            { result: Err(TooShort), rest: bytes }
        else
            bytes_before_first_field = List.drop_first(bytes, count_bytes_before_first_field)

            # Begin decoding field:value pairs
            decode_fields(initial_state, bytes_before_first_field))

object_help : ObjectState, U8 -> [Break ObjectState, Continue ObjectState]
object_help = \state, byte ->
    when (state, byte) is
        (BeforeOpeningBrace(n), b) if is_whitespace(b) -> Continue(BeforeOpeningBrace((n + 1)))
        (BeforeOpeningBrace(n), b) if b == '{' -> Continue(AfterOpeningBrace((n + 1)))
        (AfterOpeningBrace(n), b) if is_whitespace(b) -> Continue(AfterOpeningBrace((n + 1)))
        (AfterOpeningBrace(n), b) if b == '"' -> Break(ObjectFieldNameStart(n))
        (BeforeColon(n), b) if is_whitespace(b) -> Continue(BeforeColon((n + 1)))
        (BeforeColon(n), b) if b == ':' -> Continue(AfterColon((n + 1)))
        (AfterColon(n), b) if is_whitespace(b) -> Continue(AfterColon((n + 1)))
        (AfterColon(n), _) -> Break(AfterColon(n))
        (AfterObjectValue(n), b) if is_whitespace(b) -> Continue(AfterObjectValue((n + 1)))
        (AfterObjectValue(n), b) if b == ',' -> Continue(AfterComma((n + 1)))
        (AfterObjectValue(n), b) if b == '}' -> Continue(AfterClosingBrace((n + 1)))
        (AfterComma(n), b) if is_whitespace(b) -> Continue(AfterComma((n + 1)))
        (AfterComma(n), b) if b == '"' -> Break(ObjectFieldNameStart(n))
        (AfterClosingBrace(n), b) if is_whitespace(b) -> Continue(AfterClosingBrace((n + 1)))
        (AfterClosingBrace(n), _) -> Break(AfterClosingBrace(n))
        _ -> Break(InvalidObject)

ObjectState : [
    BeforeOpeningBrace U64,
    AfterOpeningBrace U64,
    ObjectFieldNameStart U64,
    BeforeColon U64,
    AfterColon U64,
    AfterObjectValue U64,
    AfterComma U64,
    AfterClosingBrace U64,
    InvalidObject,
]
