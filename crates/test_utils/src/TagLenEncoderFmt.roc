# a simple encoder/decoder format
#
# HACK: since this file is inlined into test code, it can't be a proper module
#
# the original author found it useful to leave this header here as a comment, to
# make it easy to switch back and forth between editing it as a proper roc module
# (so things like language server and `roc test` work), and inlining it into test
# code.
#
# module [
#     TagLenFmt,
#     tag_len_fmt,
# ]

TagLenFmt := {}
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

tag_len_fmt = @TagLenFmt {}

# ENCODE

append_pre_len = \bytes, pre, len ->
    List.append bytes (Num.to_u8 pre)
    |> List.concat (Num.to_str len |> Str.to_utf8)
    |> List.append ' '
encode_num = \n -> Encode.custom \bytes, @TagLenFmt {} -> append_pre_len bytes 'n' n

encode_u8 = encode_num
encode_u16 = encode_num
encode_u32 = encode_num
encode_u64 = encode_num
encode_u128 = encode_num
encode_i8 = encode_num
encode_i16 = encode_num
encode_i32 = encode_num
encode_i64 = encode_num
encode_i128 = encode_num
encode_f32 = encode_num
encode_f64 = encode_num
encode_dec = encode_num
encode_bool = \b -> encode_u8 (if b then 1 else 0)

expect
    actual = Encode.to_bytes 1 tag_len_fmt
    actual == (Str.to_utf8 "n1 ")
expect
    actual = Encode.to_bytes 1.3dec tag_len_fmt
    actual == (Str.to_utf8 "n1.3 ")
expect
    actual = Encode.to_bytes Bool.true tag_len_fmt
    actual == (Str.to_utf8 "n1 ")

encode_string = \str -> Encode.custom \bytes, @TagLenFmt {} ->
        append_pre_len bytes 's' (Str.count_utf8_bytes str)
        |> List.concat (Str.to_utf8 str)
        |> List.append ' '

expect
    actual = Encode.to_bytes "hey" tag_len_fmt
    actual == (Str.to_utf8 "s3 hey ")

encode_list = \lst, encode_elem -> Encode.custom \bytes, @TagLenFmt {} ->
        bytes_pre = append_pre_len bytes 'l' (List.len lst)
        List.walk lst bytes_pre \buf, elem ->
            Encode.append_with buf (encode_elem elem) (@TagLenFmt {})

expect
    actual = Encode.to_bytes [1, 2, 3] tag_len_fmt
    actual == (Str.to_utf8 "l3 n1 n2 n3 ")

encode_record = \fields -> Encode.custom \bytes, @TagLenFmt {} ->
        bytes_pre =
            append_pre_len bytes 'r' (List.len fields)
        List.walk fields bytes_pre \buf, { key, value } ->
            Encode.append_with buf (encode_string key) (@TagLenFmt {})
            |> Encode.append_with value (@TagLenFmt {})

expect
    actual = Encode.to_bytes { foo: "foo", bar: Bool.true } tag_len_fmt
    actual == Str.to_utf8 "r2 s3 bar n1 s3 foo s3 foo "

encode_tuple = \elems -> encode_list elems (\e -> e)
encode_tag = \name, payload -> encode_tuple (List.prepend payload (encode_string name))

expect
    actual = Encode.to_bytes (1, "foo", {}) tag_len_fmt
    actual == (Str.to_utf8 "l3 n1 s3 foo r0 ")

# DECODE

split_at_space = \bytes ->
    when List.split_first bytes ' ' is
        Ok { before, after } -> { taken: before, rest: after }
        Err _ -> { taken: [], rest: bytes }

decode_num_pre = \bytes, pre, to_num ->
    when List.split_at bytes 1 is
        { before: [b], others } if b == pre ->
            { taken, rest } = split_at_space others
            str = taken |> Str.from_utf8 |> Result.map_err \_ -> TooShort
            result = Result.try str \s -> (to_num s |> Result.map_err \_ -> TooShort)
            when result is
                Ok _ -> { result, rest }
                Err _ -> { result, rest: others }

        _ -> { result: Err TooShort, rest: bytes }

decode_num = \to_num -> Decode.custom \bytes, @TagLenFmt {} -> decode_num_pre bytes 'n' to_num

decode_u8 = decode_num Str.to_u8
decode_u16 = decode_num Str.to_u16
decode_u32 = decode_num Str.to_u32
decode_u64 = decode_num Str.to_u64
decode_u128 = decode_num Str.to_u128
decode_i8 = decode_num Str.to_i8
decode_i16 = decode_num Str.to_i16
decode_i32 = decode_num Str.to_i32
decode_i64 = decode_num Str.to_i64
decode_i128 = decode_num Str.to_i128
decode_f32 = decode_num Str.to_f32
decode_f64 = decode_num Str.to_f64
decode_dec = decode_num Str.to_dec
decode_bool = Decode.custom \bytes, @TagLenFmt {} ->
    { result: num_result, rest } = Decode.decode_with bytes decode_u8 (@TagLenFmt {})
    when num_result is
        Ok 1 -> { result: Ok Bool.true, rest }
        Ok 0 -> { result: Ok Bool.false, rest }
        _ -> { result: Err TooShort, rest: bytes }

expect
    actual = Decode.from_bytes (Str.to_utf8 "n1 ") tag_len_fmt
    actual == Ok (Num.to_u8 1)
expect
    actual = Decode.from_bytes (Str.to_utf8 "n1 ") tag_len_fmt
    actual == Ok Bool.true

decode_len_pre = \bytes, pre -> decode_num_pre bytes pre Str.to_u64

decode_try = \{ result, rest }, map ->
    when result is
        Ok a -> map a rest
        Err e -> { result: Err e, rest }

decode_string = Decode.custom \bytes, @TagLenFmt {} ->
    decode_len_pre bytes 's'
    |> decode_try \len, len_rest ->
        { before, others } = List.split_at len_rest len
        result = Str.from_utf8 before |> Result.map_err \_ -> TooShort
        when List.split_at others 1 is
            { before: [' '], others: rest } -> { result, rest }
            _ -> { result: Err TooShort, rest: others }

expect
    actual = Decode.from_bytes (Str.to_utf8 "s3 foo ") tag_len_fmt
    actual == Ok "foo"

repeat_decode : U8, List U8, state, (state -> Decode.Decoder state TagLenFmt) -> DecodeResult state
repeat_decode = \pre, bytes, state, step_state ->
    run = \end, bs ->
        List.range { start: At 0, end: Before end }
        |> List.walk { result: Ok state, rest: bs } \res, _i ->
            decode_try res \s, rest ->
                Decode.decode_with rest (step_state s) (@TagLenFmt {})

    decode_len_pre bytes pre |> decode_try run

decode_list = \elem_decoder -> Decode.custom \bytes, @TagLenFmt {} ->
        step = \lst -> Decode.custom \sbytes, @TagLenFmt {} ->
                Decode.decode_with sbytes elem_decoder (@TagLenFmt {})
                |> Decode.map_result \elem -> List.append lst elem
        repeat_decode 'l' bytes [] step

expect
    actual = Decode.from_bytes (Str.to_utf8 "l3 n1 n2 n3 ") tag_len_fmt
    actual == Ok [1, 2, 3]

decode_record = \init_state, step_field, finalizer -> Decode.custom \bytes, @TagLenFmt {} ->
        flatten_field_res = \next, rest ->
            when next is
                Keep value_decoder -> { result: Ok value_decoder, rest }
                Skip -> { result: Err TooShort, rest }

        step = \state -> Decode.custom \sbytes, @TagLenFmt {} ->
                Decode.decode_with sbytes decode_string (@TagLenFmt {})
                |> decode_try \key, bs ->
                    flatten_field_res (step_field state key) bs
                |> decode_try \value_decoder, bs ->
                    Decode.decode_with bs value_decoder (@TagLenFmt {})

        repeat_decode 'r' bytes init_state step
        |> decode_try \state, rest -> { result: finalizer state (@TagLenFmt {}), rest }

expect
    actual = Decode.from_bytes (Str.to_utf8 "r2 s3 bar n1 s3 foo s3 foo ") tag_len_fmt
    actual == Ok ({ foo: "foo", bar: Bool.true })

decode_tuple = \initial_state, step_elem, finalizer -> Decode.custom \bytes, @TagLenFmt {} ->
        flatten_field_res = \next, rest ->
            when next is
                Next dec -> { result: Ok dec, rest }
                TooLong -> { result: Err TooShort, rest }
        step = \{ state, i } -> Decode.custom \sbytes, @TagLenFmt {} ->
                flatten_field_res (step_elem state i) sbytes
                |> decode_try \dec, rest -> Decode.decode_with rest dec (@TagLenFmt {})
                |> Decode.map_result \s -> { state: s, i: i + 1 }

        repeat_decode 'l' bytes { state: initial_state, i: 0 } step
        |> decode_try \s, rest -> { result: finalizer s.state, rest }

expect
    actual = Decode.from_bytes (Str.to_utf8 "l3 n1 s3 abc l1 n0 ") tag_len_fmt
    actual == Ok (1, "abc", [Bool.false])

expect
    input = { foo: (1, "abc", [Bool.false, Bool.true]), bar: { baz: 0.32 } }
    encoded = Encode.to_bytes input tag_len_fmt
    decoded = Decode.from_bytes encoded tag_len_fmt
    decoded == Ok input
