module [
    Encoder,
    Encoding,
    to_encoder,
    EncoderFormatting,
    u8,
    u16,
    u32,
    u64,
    u128,
    i8,
    i16,
    i32,
    i64,
    i128,
    f32,
    f64,
    dec,
    bool,
    string,
    list,
    record,
    tag,
    tuple,
    custom,
    append_with,
    append,
    to_bytes,
]

import Num exposing [
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    Dec,
]
import Bool exposing [Bool]

Encoder fmt := List U8, fmt -> List U8 where fmt implements EncoderFormatting

Encoding implements
    to_encoder : val -> Encoder fmt where val implements Encoding, fmt implements EncoderFormatting

EncoderFormatting implements
    u8 : U8 -> Encoder fmt where fmt implements EncoderFormatting
    u16 : U16 -> Encoder fmt where fmt implements EncoderFormatting
    u32 : U32 -> Encoder fmt where fmt implements EncoderFormatting
    u64 : U64 -> Encoder fmt where fmt implements EncoderFormatting
    u128 : U128 -> Encoder fmt where fmt implements EncoderFormatting
    i8 : I8 -> Encoder fmt where fmt implements EncoderFormatting
    i16 : I16 -> Encoder fmt where fmt implements EncoderFormatting
    i32 : I32 -> Encoder fmt where fmt implements EncoderFormatting
    i64 : I64 -> Encoder fmt where fmt implements EncoderFormatting
    i128 : I128 -> Encoder fmt where fmt implements EncoderFormatting
    f32 : F32 -> Encoder fmt where fmt implements EncoderFormatting
    f64 : F64 -> Encoder fmt where fmt implements EncoderFormatting
    dec : Dec -> Encoder fmt where fmt implements EncoderFormatting
    bool : Bool -> Encoder fmt where fmt implements EncoderFormatting
    string : Str -> Encoder fmt where fmt implements EncoderFormatting
    list : List elem, (elem -> Encoder fmt) -> Encoder fmt where fmt implements EncoderFormatting
    record : List { key : Str, value : Encoder fmt } -> Encoder fmt where fmt implements EncoderFormatting
    tuple : List (Encoder fmt) -> Encoder fmt where fmt implements EncoderFormatting
    tag : Str, List (Encoder fmt) -> Encoder fmt where fmt implements EncoderFormatting

## Creates a custom encoder from a given function.
##
## ```roc
## expect
##     # Appends the byte 42
##     custom_encoder = Encode.custom(\bytes, _fmt -> List.append(bytes, 42))
##
##     actual = Encode.append_with([], custom_encoder, Core.json)
##     expected = [42] # Expected result is a list with a single byte, 42
##
##     actual == expected
## ```
custom : (List U8, fmt -> List U8) -> Encoder fmt where fmt implements EncoderFormatting
custom = \encoder -> @Encoder(encoder)

append_with : List U8, Encoder fmt, fmt -> List U8 where fmt implements EncoderFormatting
append_with = \lst, @Encoder(do_encoding), fmt -> do_encoding(lst, fmt)

## Appends the encoded representation of a value to an existing list of bytes.
##
## ```roc
## expect
##     actual = Encode.append([], { foo: 43 }, Core.json)
##     expected = Str.to_utf8("""{"foo":43}""")
##
##     actual == expected
## ```
append : List U8, val, fmt -> List U8 where val implements Encoding, fmt implements EncoderFormatting
append = \lst, val, fmt -> append_with(lst, to_encoder(val), fmt)

## Encodes a value to a list of bytes (`List U8`) according to the specified format.
##
## ```roc
## expect
##     foo_rec = { foo: 42 }
##
##     actual = Encode.to_bytes(foo_rec, Core.json)
##     expected = Str.to_utf8("""{"foo":42}""")
##
##     actual == expected
## ```
to_bytes : val, fmt -> List U8 where val implements Encoding, fmt implements EncoderFormatting
to_bytes = \val, fmt -> append_with([], to_encoder(val), fmt)
