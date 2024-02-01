interface Encode
    exposes [
        Encoder,
        Encoding,
        toEncoder,
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
        appendWith,
        append,
        toBytes,
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
    toEncoder : val -> Encoder fmt where val implements Encoding, fmt implements EncoderFormatting

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

custom : (List U8, fmt -> List U8) -> Encoder fmt where fmt implements EncoderFormatting
custom = \encoder -> @Encoder encoder

appendWith : List U8, Encoder fmt, fmt -> List U8 where fmt implements EncoderFormatting
appendWith = \lst, @Encoder doEncoding, fmt -> doEncoding lst fmt

append : List U8, val, fmt -> List U8 where val implements Encoding, fmt implements EncoderFormatting
append = \lst, val, fmt -> appendWith lst (toEncoder val) fmt

toBytes : val, fmt -> List U8 where val implements Encoding, fmt implements EncoderFormatting
toBytes = \val, fmt -> appendWith [] (toEncoder val) fmt
