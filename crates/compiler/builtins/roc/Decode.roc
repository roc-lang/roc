module [
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

import List
import Result exposing [Result]
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

## Return type of a [Decoder].
##
## This can be useful when creating a [custom](#custom) decoder or when
## using [fromBytesPartial](#fromBytesPartial). For example writing unit tests,
## such as;
## ```roc
## expect
##     input = "\"hello\", " |> Str.toUtf8
##     actual = Decode.fromBytesPartial input Json.json
##     expected = Ok "hello"
##
##     actual.result == expected
## ```
DecodeResult input val err : { result : Result val err, rest : input }

## Decodes a `input` of utf-8 bytes where `val` is the type of the decoded
## value, and `fmt` is a [Decoder] which implements the [DecoderFormatting]
## ability
Decoder input val fmt err := input, fmt -> DecodeResult input val err where fmt implements DecoderFormatting

## Definition of the [Decoding] ability
Decoding implements
    decoder : Decoder input val fmt err where val implements Decoding, fmt implements DecoderFormatting

## Definition of the [DecoderFormatting] ability
DecoderFormatting implements
    u8 : Decoder input U8 fmt err where fmt implements DecoderFormatting
    u16 : Decoder input U16 fmt err where fmt implements DecoderFormatting
    u32 : Decoder input U32 fmt err where fmt implements DecoderFormatting
    u64 : Decoder input U64 fmt err where fmt implements DecoderFormatting
    u128 : Decoder input U128 fmt err where fmt implements DecoderFormatting
    i8 : Decoder input I8 fmt err where  fmt implements DecoderFormatting
    i16 : Decoder input I16 fmt err where fmt implements DecoderFormatting
    i32 : Decoder input I32 fmt err where fmt implements DecoderFormatting
    i64 : Decoder input I64 fmt err where fmt implements DecoderFormatting
    i128 : Decoder input I128 fmt err where fmt implements DecoderFormatting
    f32 : Decoder input F32 fmt err where fmt implements DecoderFormatting
    f64 : Decoder input F64 fmt err where fmt implements DecoderFormatting
    dec : Decoder input Dec fmt err where fmt implements DecoderFormatting
    bool : Decoder input Bool fmt err where fmt implements DecoderFormatting
    string : Decoder input Str fmt err where fmt implements DecoderFormatting
    list : Decoder input elem fmt err -> Decoder input (List elem) fmt err where fmt implements DecoderFormatting

    ## `record state stepField finalizer` decodes a record field-by-field.
    ##
    ## `stepField` returns a decoder for the given field in the record, or
    ## `Skip` if the field is not a part of the decoded record.
    ##
    ## `finalizer` should produce the record value from the decoded `state`.
    record : state, (state, Str -> [Keep (Decoder input state fmt err), Skip]), (state, fmt -> Result val err) -> Decoder input val fmt err where fmt implements DecoderFormatting

    ## `tuple state stepElem finalizer` decodes a tuple element-by-element.
    ##
    ## `stepElem` returns a decoder for the nth index in the tuple, or
    ## `TooLong` if the index is larger than the expected size of the tuple. The
    ## index passed to `stepElem` is 0-indexed.
    ##
    ## `finalizer` should produce the tuple value from the decoded `state`.
    tuple : state, (state, U64 -> [Next (Decoder input state fmt err), TooLong]), (state -> Result val err) -> Decoder input val fmt err where fmt implements DecoderFormatting

## Build a custom [Decoder] function. For example the implementation of
## `decodeBool` could be defined as follows;
##
## ```roc
## decodeBool = Decode.custom \bytes, @Json {} ->
##     when bytes is
##         ['f', 'a', 'l', 's', 'e', ..] -> { result: Ok Bool.false, rest: List.dropFirst bytes 5 }
##         ['t', 'r', 'u', 'e', ..] -> { result: Ok Bool.true, rest: List.dropFirst bytes 4 }
##         _ -> { result: Err TooShort, rest: bytes }
## ```
custom : (input, fmt -> DecodeResult input val err) -> Decoder input val fmt err where fmt implements DecoderFormatting
custom = \decode -> @Decoder decode

## Decode a `input` utf-8 bytes using a specific [Decoder] function
decodeWith : input, Decoder input val fmt err, fmt -> DecodeResult input val err where fmt implements DecoderFormatting
decodeWith = \bytes, @Decoder decode, fmt -> decode bytes fmt

## Decode a `input` utf-8 bytes and return a [DecodeResult](#DecodeResult)
## ```roc
## expect
##     input = "\"hello\", " |> Str.toUtf8
##     actual = Decode.fromBytesPartial input Json.json
##     expected = Ok "hello"
##
##     actual.result == expected
## ```
fromBytesPartial : input, fmt -> DecodeResult input val err where val implements Decoding, fmt implements DecoderFormatting
fromBytesPartial = \bytes, fmt -> decodeWith bytes decoder fmt

## Decode a `input` utf-8 bytes and return a [Result] with no leftover bytes
## expected. If successful returns `Ok val`, however, if there are bytes
## remaining returns `Err Leftover (input)`.
## ```roc
## expect
##     input = "\"hello\", " |> Str.toUtf8
##     actual = Decode.fromBytes input Json.json
##     expected = Ok "hello"
##
##     actual == expected
## ```
fromBytes : List U8, fmt -> Result val [Leftover (List U8)] where val implements Decoding, fmt implements DecoderFormatting
fromBytes = \bytes, fmt ->
    when fromBytesPartial bytes fmt is
        { result, rest } ->
            if List.isEmpty rest then
                when result is
                    Ok val -> Ok val
                    Err e -> Err e
            else
                Err (Leftover rest)

## Transform the `val` of a [DecodeResult]
mapResult : DecodeResult input a err, (a -> b) -> DecodeResult input b err
mapResult = \{ result, rest }, mapper -> { result: Result.map result mapper, rest }
