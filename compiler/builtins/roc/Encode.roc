interface Encode
    exposes
        [
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
            custom,
            appendWith,
            append,
            toBytes,
        ]
    imports
        []

Encoder fmt := List U8, fmt -> List U8 | fmt has EncoderFormatting

Encoding has
    toEncoder : val -> Encoder fmt | val has Encoding, fmt has EncoderFormatting

EncoderFormatting has
    u8 : U8 -> Encoder fmt | fmt has EncoderFormatting
    u16 : U16 -> Encoder fmt | fmt has EncoderFormatting
    u32 : U32 -> Encoder fmt | fmt has EncoderFormatting
    u64 : U64 -> Encoder fmt | fmt has EncoderFormatting
    u128 : U128 -> Encoder fmt | fmt has EncoderFormatting
    i8 : I8 -> Encoder fmt | fmt has EncoderFormatting
    i16 : I16 -> Encoder fmt | fmt has EncoderFormatting
    i32 : I32 -> Encoder fmt | fmt has EncoderFormatting
    i64 : I64 -> Encoder fmt | fmt has EncoderFormatting
    i128 : I128 -> Encoder fmt | fmt has EncoderFormatting
    f32 : F32 -> Encoder fmt | fmt has EncoderFormatting
    f64 : F64 -> Encoder fmt | fmt has EncoderFormatting
    dec : Dec -> Encoder fmt | fmt has EncoderFormatting
    bool : Bool -> Encoder fmt | fmt has EncoderFormatting
    string : Str -> Encoder fmt | fmt has EncoderFormatting
    list : List elem, (elem -> Encoder fmt) -> Encoder fmt | fmt has EncoderFormatting
    record : List { key : Str, value : Encoder fmt } -> Encoder fmt | fmt has EncoderFormatting
    tag : Str, List (Encoder fmt) -> Encoder fmt | fmt has EncoderFormatting

custom : (List U8, fmt -> List U8) -> Encoder fmt | fmt has EncoderFormatting
custom = \encoder -> @Encoder encoder

appendWith : List U8, Encoder fmt, fmt -> List U8 | fmt has EncoderFormatting
appendWith = \lst, @Encoder doEncoding, fmt -> doEncoding lst fmt

append : List U8, val, fmt -> List U8 | val has Encoding, fmt has EncoderFormatting
append = \lst, val, fmt -> appendWith lst (toEncoder val) fmt

toBytes : val, fmt -> List U8 | val has Encoding, fmt has EncoderFormatting
toBytes = \val, fmt -> appendWith [] (toEncoder val) fmt
