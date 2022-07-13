interface Decode
    exposes [
        DecodeError,
        DecodeResult,
        Decoder,
        Decoding,
        DecoderFormatting,
        decoder,
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
        custom,
        decodeWith,
        fromBytesPartial,
        fromBytes,
    ]
    imports [
        List,
    ]

DecodeError : [TooShort]

DecodeResult val : { result : Result val DecodeError, rest : List U8 }

Decoder val fmt := List U8, fmt -> DecodeResult val | fmt has DecoderFormatting

Decoding has
    decoder : Decoder val fmt | val has Decoding, fmt has DecoderFormatting

DecoderFormatting has
    u8 : Decoder U8 fmt | fmt has DecoderFormatting
    u16 : Decoder U16 fmt | fmt has DecoderFormatting
    u32 : Decoder U32 fmt | fmt has DecoderFormatting
    u64 : Decoder U64 fmt | fmt has DecoderFormatting
    u128 : Decoder U128 fmt | fmt has DecoderFormatting
    i8 : Decoder I8 fmt | fmt has DecoderFormatting
    i16 : Decoder I16 fmt | fmt has DecoderFormatting
    i32 : Decoder I32 fmt | fmt has DecoderFormatting
    i64 : Decoder I64 fmt | fmt has DecoderFormatting
    i128 : Decoder I128 fmt | fmt has DecoderFormatting
    f32 : Decoder F32 fmt | fmt has DecoderFormatting
    f64 : Decoder F64 fmt | fmt has DecoderFormatting
    dec : Decoder Dec fmt | fmt has DecoderFormatting
    bool : Decoder Bool fmt | fmt has DecoderFormatting
    string : Decoder Str fmt | fmt has DecoderFormatting
    list : Decoder elem fmt -> Decoder (List elem) fmt | fmt has DecoderFormatting

custom : (List U8, fmt -> DecodeResult val) -> Decoder val fmt | fmt has DecoderFormatting
custom = \decode -> @Decoder decode

decodeWith : List U8, Decoder val fmt, fmt -> DecodeResult val | fmt has DecoderFormatting
decodeWith = \bytes, @Decoder decode, fmt -> decode bytes fmt

fromBytesPartial : List U8, fmt -> DecodeResult val | val has Decoding, fmt has DecoderFormatting
fromBytesPartial = \bytes, fmt -> decodeWith bytes decoder fmt

fromBytes : List U8, fmt -> Result val [Leftover (List U8)]DecodeError | val has Decoding, fmt has DecoderFormatting
fromBytes = \bytes, fmt ->
    { result, rest } = fromBytesPartial bytes fmt

    if List.isEmpty rest then
        when result is
            Ok val -> Ok val
            Err TooShort -> Err TooShort
    else
        Err (Leftover rest)
