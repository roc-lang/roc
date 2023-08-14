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
        record,
        tuple,
        custom,
        decodeWith,
        fromBytesPartial,
        fromBytes,
        mapResult,
    ]
    imports [
        List,
        Result.{ Result },
        Num.{
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
            Nat,
            F32,
            F64,
            Dec,
        },
        Bool.{ Bool },
    ]

## Error types when decoding a `List U8` of utf-8 bytes using a [Decoder]
DecodeError : [TooShort]

## Return type of a [Decoder].
##
## This is can be useful when creating a [custom](#custom) decoder or when
## using [fromBytesPartial](#fromBytesPartial). For example writing unit tests,
## such as;
## ```
## expect
##     input = "\"hello\", " |> Str.toUtf8
##     actual = Decode.fromBytesPartial input Json.json
##     expected = Ok "hello"
##
##     actual.result == expected
## ```
DecodeResult val : { result : Result val DecodeError, rest : List U8 }

## Decodes a `List U8` of utf-8 bytes where `val` is the type of the decoded
## value, and `fmt` is a [Decoder] which implements the [DecoderFormatting]
## ability
Decoder val fmt := List U8, fmt -> DecodeResult val where fmt implements DecoderFormatting

## Definition of the [Decoding] ability
Decoding implements
    decoder : Decoder val fmt where val implements Decoding, fmt implements DecoderFormatting

## Definition of the [DecoderFormatting] ability
DecoderFormatting implements
    u8 : Decoder U8 fmt where fmt implements DecoderFormatting
    u16 : Decoder U16 fmt where fmt implements DecoderFormatting
    u32 : Decoder U32 fmt where fmt implements DecoderFormatting
    u64 : Decoder U64 fmt where fmt implements DecoderFormatting
    u128 : Decoder U128 fmt where fmt implements DecoderFormatting
    i8 : Decoder I8 fmt where fmt implements DecoderFormatting
    i16 : Decoder I16 fmt where fmt implements DecoderFormatting
    i32 : Decoder I32 fmt where fmt implements DecoderFormatting
    i64 : Decoder I64 fmt where fmt implements DecoderFormatting
    i128 : Decoder I128 fmt where fmt implements DecoderFormatting
    f32 : Decoder F32 fmt where fmt implements DecoderFormatting
    f64 : Decoder F64 fmt where fmt implements DecoderFormatting
    dec : Decoder Dec fmt where fmt implements DecoderFormatting
    bool : Decoder Bool fmt where fmt implements DecoderFormatting
    string : Decoder Str fmt where fmt implements DecoderFormatting
    list : Decoder elem fmt -> Decoder (List elem) fmt where fmt implements DecoderFormatting

    ## `record state stepField finalizer` decodes a record field-by-field.
    ##
    ## `stepField` returns a decoder for the given field in the record, or
    ## `Skip` if the field is not a part of the decoded record.
    ##
    ## `finalizer` should produce the record value from the decoded `state`.
    record : state, (state, Str -> [Keep (Decoder state fmt), Skip]), (state -> Result val DecodeError) -> Decoder val fmt where fmt implements DecoderFormatting

    ## `tuple state stepElem finalizer` decodes a tuple element-by-element.
    ##
    ## `stepElem` returns a decoder for the nth index in the tuple, or
    ## `TooLong` if the index is larger than the expected size of the tuple. The
    ## index passed to `stepElem` is 0-indexed.
    ##
    ## `finalizer` should produce the tuple value from the decoded `state`.
    tuple : state, (state, Nat -> [Next (Decoder state fmt), TooLong]), (state -> Result val DecodeError) -> Decoder val fmt where fmt implements DecoderFormatting

## Build a custom [Decoder] function. For example the implementation of
## `decodeBool` could be defined as follows;
##
## ```
## decodeBool = Decode.custom \bytes, @Json {} ->
##     when bytes is
##         ['f', 'a', 'l', 's', 'e', ..] -> { result: Ok Bool.false, rest: List.drop bytes 5 }
##         ['t', 'r', 'u', 'e', ..] -> { result: Ok Bool.true, rest: List.drop bytes 4 }
##         _ -> { result: Err TooShort, rest: bytes }
## ```
custom : (List U8, fmt -> DecodeResult val) -> Decoder val fmt where fmt implements DecoderFormatting
custom = \decode -> @Decoder decode

## Decode a `List U8` utf-8 bytes using a specific [Decoder] function
decodeWith : List U8, Decoder val fmt, fmt -> DecodeResult val where fmt implements DecoderFormatting
decodeWith = \bytes, @Decoder decode, fmt -> decode bytes fmt

## Decode a `List U8` utf-8 bytes and return a [DecodeResult](#DecodeResult)
## ```
## expect
##     input = "\"hello\", " |> Str.toUtf8
##     actual = Decode.fromBytesPartial input Json.json
##     expected = Ok "hello"
##
##     actual.result == expected
## ```
fromBytesPartial : List U8, fmt -> DecodeResult val where val implements Decoding, fmt implements DecoderFormatting
fromBytesPartial = \bytes, fmt -> decodeWith bytes decoder fmt

## Decode a `List U8` utf-8 bytes and return a [Result] with no leftover bytes
## expected. If successful returns `Ok val`, however, if there are bytes
## remaining returns `Err Leftover (List U8)`.
## ```
## expect
##     input = "\"hello\", " |> Str.toUtf8
##     actual = Decode.fromBytes input Json.json
##     expected = Ok "hello"
##
##     actual == expected
## ```
fromBytes : List U8, fmt -> Result val [Leftover (List U8)]DecodeError where val implements Decoding, fmt implements DecoderFormatting
fromBytes = \bytes, fmt ->
    when fromBytesPartial bytes fmt is
        { result, rest } ->
            if List.isEmpty rest then
                when result is
                    Ok val -> Ok val
                    Err TooShort -> Err TooShort
            else
                Err (Leftover rest)

## Transform the `val` of a [DecodeResult]
mapResult : DecodeResult a, (a -> b) -> DecodeResult b
mapResult = \{ result, rest }, mapper -> { result: Result.map result mapper, rest }
