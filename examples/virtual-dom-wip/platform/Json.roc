module [
    Json,
    json,
    jsonWithOptions,
]

import Encode exposing [appendWith]

## An opaque type with the `EncoderFormatting` and
## `DecoderFormatting` abilities.
Json := {}
    implements [
        EncoderFormatting {
            u8: encodeU8,
            u16: encodeU16,
            u32: encodeU32,
            u64: encodeU64,
            u128: encodeU128,
            i8: encodeI8,
            i16: encodeI16,
            i32: encodeI32,
            i64: encodeI64,
            i128: encodeI128,
            f32: encodeF32,
            f64: encodeF64,
            dec: encodeDec,
            bool: encodeBool,
            string: encodeString,
            list: encodeList,
            record: encodeRecord,
            tuple: encodeTuple,
            tag: encodeTag,
        },
        DecoderFormatting {
            u8: decodeU8,
            u16: decodeU16,
            u32: decodeU32,
            u64: decodeU64,
            u128: decodeU128,
            i8: decodeI8,
            i16: decodeI16,
            i32: decodeI32,
            i64: decodeI64,
            i128: decodeI128,
            f32: decodeF32,
            f64: decodeF64,
            dec: decodeDec,
            bool: decodeBool,
            string: decodeString,
            list: decodeList,
            record: decodeRecord,
            tuple: decodeTuple,
        },
    ]

## Returns a JSON `Encoder` and `Decoder`
json = @Json {}

## Returns a JSON `Encoder` and `Decoder` with configuration options
jsonWithOptions = \{} ->
    @Json {}

# TODO encode as JSON numbers as base 10 decimal digits
# e.g. the REPL `Num.toStr 12e42f64` gives
# "12000000000000000000000000000000000000000000" : Str
# which should be encoded as "12e42" : Str
numToBytes = \n ->
    n |> Num.toStr |> Str.toUtf8

encodeU8 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeU16 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeU32 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeU64 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeU128 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI8 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI16 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI32 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI64 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI128 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeF32 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeF64 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeDec = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeBool = \b ->
    Encode.custom \bytes, @Json {} ->
        if b then
            List.concat bytes (Str.toUtf8 "true")
        else
            List.concat bytes (Str.toUtf8 "false")

encodeString = \str ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (encodeStrBytes str)

# TODO add support for unicode escapes (including 2,3,4 byte code points)
# these should be encoded using a 12-byte sequence encoding the UTF-16 surrogate
# pair. For example a string containing only G clef character U+1D11E is
# represented as "\\uD834\\uDD1E" (note "\\" here is a single reverse solidus)
encodeStrBytes = \str ->
    bytes = Str.toUtf8 str

    initialState = { bytePos: 0, status: NoEscapesFound }

    firstPassState =
        List.walkUntil bytes initialState \{ bytePos, status }, b ->
            when b is
                0x22 -> Break { bytePos, status: FoundEscape } # U+0022 Quotation mark
                0x5c -> Break { bytePos, status: FoundEscape } # U+005c Reverse solidus
                0x2f -> Break { bytePos, status: FoundEscape } # U+002f Solidus
                0x08 -> Break { bytePos, status: FoundEscape } # U+0008 Backspace
                0x0c -> Break { bytePos, status: FoundEscape } # U+000c Form feed
                0x0a -> Break { bytePos, status: FoundEscape } # U+000a Line feed
                0x0d -> Break { bytePos, status: FoundEscape } # U+000d Carriage return
                0x09 -> Break { bytePos, status: FoundEscape } # U+0009 Tab
                _ -> Continue { bytePos: bytePos + 1, status }

    when firstPassState.status is
        NoEscapesFound ->
            (List.len bytes)
            + 2
            |> List.withCapacity
            |> List.concat ['"']
            |> List.concat bytes
            |> List.concat ['"']

        FoundEscape ->
            { before: bytesBeforeEscape, others: bytesWithEscapes } =
                List.splitAt bytes firstPassState.bytePos

            # Reserve List with 120% capacity for escaped bytes to reduce
            # allocations, include starting quote, and bytes up to first escape
            initial =
                List.len bytes
                |> Num.mul 120
                |> Num.divCeil 100
                |> List.withCapacity
                |> List.concat ['"']
                |> List.concat bytesBeforeEscape

            # Walk the remaining bytes and include escape '\' as required
            # add closing quote
            List.walk bytesWithEscapes initial \encodedBytes, byte ->
                List.concat encodedBytes (escapedByteToJson byte)
            |> List.concat ['"']

# Prepend an "\" escape byte
escapedByteToJson : U8 -> List U8
escapedByteToJson = \b ->
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

encodeList = \lst, encodeElem ->
    Encode.custom \bytes, @Json {} ->
        writeList = \{ buffer, elemsLeft }, elem ->
            beforeBufferLen = buffer |> List.len

            bufferWithElem = appendWith buffer (encodeElem elem) (@Json {})
            # If our encoder returned [] we just skip the elem
            if bufferWithElem |> List.len == beforeBufferLen then
                { buffer: bufferWithElem, elemsLeft: elemsLeft - 1 }
            else
                bufferWithSuffix =
                    if elemsLeft > 1 then
                        List.append bufferWithElem (Num.toU8 ',')
                    else
                        bufferWithElem

                { buffer: bufferWithSuffix, elemsLeft: elemsLeft - 1 }

        head = List.append bytes (Num.toU8 '[')
        { buffer: withList } = List.walk lst { buffer: head, elemsLeft: List.len lst } writeList

        List.append withList (Num.toU8 ']')

encodeRecord = \fields ->
    Encode.custom \bytes, @Json {} ->
        writeRecord = \{ buffer, fieldsLeft }, { key, value } ->

            fieldValue = [] |> appendWith value (json)
            # If our encoder returned [] we just skip the field
            if fieldValue == [] then
                { buffer, fieldsLeft: fieldsLeft - 1 }
            else
                fieldName = key
                bufferWithKeyValue =
                    List.append buffer (Num.toU8 '"')
                    |> List.concat (Str.toUtf8 fieldName)
                    |> List.append (Num.toU8 '"')
                    |> List.append (Num.toU8 ':') # Note we need to encode using the json config here
                    |> List.concat fieldValue

                bufferWithSuffix =
                    if fieldsLeft > 1 then
                        List.append bufferWithKeyValue (Num.toU8 ',')
                    else
                        bufferWithKeyValue

                { buffer: bufferWithSuffix, fieldsLeft: fieldsLeft - 1 }

        bytesHead = List.append bytes (Num.toU8 '{')
        { buffer: bytesWithRecord } = List.walk fields { buffer: bytesHead, fieldsLeft: List.len fields } writeRecord

        List.append bytesWithRecord (Num.toU8 '}')

encodeTuple = \elems ->
    Encode.custom \bytes, @Json {} ->
        writeTuple = \{ buffer, elemsLeft }, elemEncoder ->
            beforeBufferLen = buffer |> List.len

            bufferWithElem = appendWith buffer (elemEncoder) (@Json {})

            # If our encoder returned [] we just skip the elem
            if bufferWithElem |> List.len == beforeBufferLen then
                { buffer: bufferWithElem, elemsLeft: elemsLeft - 1 }
            else
                bufferWithSuffix =
                    if elemsLeft > 1 then
                        List.append bufferWithElem (Num.toU8 ',')
                    else
                        bufferWithElem

                { buffer: bufferWithSuffix, elemsLeft: elemsLeft - 1 }

        bytesHead = List.append bytes (Num.toU8 '[')
        { buffer: bytesWithRecord } = List.walk elems { buffer: bytesHead, elemsLeft: List.len elems } writeTuple

        List.append bytesWithRecord (Num.toU8 ']')
encodeTag = \name, payload ->
    Encode.custom \bytes, @Json {} ->
        # Idea: encode `A v1 v2` as `{"A": [v1, v2]}`
        writePayload = \{ buffer, itemsLeft }, encoder ->
            bufferWithValue = appendWith buffer encoder (@Json {})
            bufferWithSuffix =
                if itemsLeft > 1 then
                    List.append bufferWithValue (Num.toU8 ',')
                else
                    bufferWithValue

            { buffer: bufferWithSuffix, itemsLeft: itemsLeft - 1 }

        bytesHead =
            List.append bytes (Num.toU8 '{')
            |> List.append (Num.toU8 '"')
            |> List.concat (Str.toUtf8 name)
            |> List.append (Num.toU8 '"')
            |> List.append (Num.toU8 ':')
            |> List.append (Num.toU8 '[')

        { buffer: bytesWithPayload } = List.walk payload { buffer: bytesHead, itemsLeft: List.len payload } writePayload

        List.append bytesWithPayload (Num.toU8 ']')
        |> List.append (Num.toU8 '}')

decodeU8 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU8
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of U8
expect
    actual = Str.toUtf8 "255" |> Decode.fromBytes json
    actual == Ok 255u8

decodeU16 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU16
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of U16
expect
    actual = Str.toUtf8 "65535" |> Decode.fromBytes json
    actual == Ok 65_535u16

decodeU32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU32
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of U32
expect
    actual = Str.toUtf8 "4000000000" |> Decode.fromBytes json
    actual == Ok 4_000_000_000u32

decodeU64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU64
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of U64
expect
    actual = Str.toUtf8 "18446744073709551614" |> Decode.fromBytes json
    actual == Ok 18_446_744_073_709_551_614u64

decodeU128 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU128
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of U128
expect
    actual = Str.toUtf8 "1234567" |> Decode.fromBytesPartial json
    actual.result == Ok 1234567u128

# TODO should we support decoding bigints, note that valid json is only a
# double precision float-64
# expect
#     actual = Str.toUtf8 "340282366920938463463374607431768211455" |> Decode.fromBytesPartial json
#     actual.result == Ok 340_282_366_920_938_463_463_374_607_431_768_211_455u128

decodeI8 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI8
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of I8
expect
    actual = Str.toUtf8 "-125" |> Decode.fromBytesPartial json
    actual.result == Ok -125i8

decodeI16 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI16
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of I16
expect
    actual = Str.toUtf8 "-32768" |> Decode.fromBytesPartial json
    actual.result == Ok -32_768i16

decodeI32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI32
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of I32
expect
    actual = Str.toUtf8 "-2147483648" |> Decode.fromBytesPartial json
    actual.result == Ok -2_147_483_648i32

decodeI64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI64
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of I64
expect
    actual = Str.toUtf8 "-9223372036854775808" |> Decode.fromBytesPartial json
    actual.result == Ok -9_223_372_036_854_775_808i64

decodeI128 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI128
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of I128
# expect
#     actual = Str.toUtf8 "-170141183460469231731687303715884105728" |> Decode.fromBytesPartial json
#     actual.result == Ok -170_141_183_460_469_231_731_687_303_715_884_105_728i128

decodeF32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toF32
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of F32
expect
    actual : DecodeResult F32
    actual = Str.toUtf8 "12.34e-5" |> Decode.fromBytesPartial json
    numStr = actual.result |> Result.map Num.toStr

    Result.withDefault numStr "" == "0.00012339999375399202"

decodeF64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toF64
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of F64
expect
    actual : DecodeResult F64
    actual = Str.toUtf8 "12.34e-5" |> Decode.fromBytesPartial json
    numStr = actual.result |> Result.map Num.toStr

    Result.withDefault numStr "" == "0.0001234"

decodeDec = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toDec
        |> Result.mapErr \_ -> TooShort

    { result, rest }

# Test decode of Dec
expect
    actual : DecodeResult Dec
    actual = Str.toUtf8 "12.0034" |> Decode.fromBytesPartial json

    actual.result == Ok 12.0034dec

decodeBool = Decode.custom \bytes, @Json {} ->
    when bytes is
        ['f', 'a', 'l', 's', 'e', ..] -> { result: Ok Bool.false, rest: List.dropFirst bytes 5 }
        ['t', 'r', 'u', 'e', ..] -> { result: Ok Bool.true, rest: List.dropFirst bytes 4 }
        _ -> { result: Err TooShort, rest: bytes }

# Test decode of Bool
expect
    actual = "true\n" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Ok Bool.true
    actual.result == expected

# Test decode of Bool
expect
    actual = "false ]\n" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Ok Bool.false
    actual.result == expected

decodeTuple = \initialState, stepElem, finalizer -> Decode.custom \initialBytes, @Json {} ->
        # NB: the stepper function must be passed explicitly until #2894 is resolved.
        decodeElems = \stepper, state, index, bytes ->
            (
                when stepper state index is
                    TooLong ->
                        bytes
                        |> anything
                        |> tryDecode \{ rest: beforeCommaOrBreak } ->
                            { result: Ok state, rest: beforeCommaOrBreak }

                    Next decoder ->
                        Decode.decodeWith bytes decoder json
            )
            |> tryDecode \{ val: newState, rest: beforeCommaOrBreak } ->
                { result: commaResult, rest: nextBytes } = comma beforeCommaOrBreak

                when commaResult is
                    Ok {} -> decodeElems stepElem newState (index + 1) nextBytes
                    Err _ -> { result: Ok newState, rest: nextBytes }

        initialBytes
        |> openBracket
        |> tryDecode \{ rest: afterBracketBytes } ->
            decodeElems stepElem initialState 0 afterBracketBytes
            |> tryDecode \{ val: endStateResult, rest: beforeClosingBracketBytes } ->
                beforeClosingBracketBytes
                |> closingBracket
                |> tryDecode \{ rest: afterTupleBytes } ->
                    when finalizer endStateResult is
                        Ok val -> { result: Ok val, rest: afterTupleBytes }
                        Err e -> { result: Err e, rest: afterTupleBytes }

# Test decode of tuple
expect
    input = Str.toUtf8 "[\"The Answer is\",42]"
    actual = Decode.fromBytesPartial input json

    actual.result == Ok ("The Answer is", 42)

parseExactChar : List U8, U8 -> DecodeResult {}
parseExactChar = \bytes, char ->
    when List.get bytes 0 is
        Ok c ->
            if
                c == char
            then
                { result: Ok {}, rest: (List.splitAt bytes 1).others }
            else
                { result: Err TooShort, rest: bytes }

        Err _ -> { result: Err TooShort, rest: bytes }

openBracket : List U8 -> DecodeResult {}
openBracket = \bytes -> parseExactChar bytes '['

closingBracket : List U8 -> DecodeResult {}
closingBracket = \bytes -> parseExactChar bytes ']'

anything : List U8 -> DecodeResult {}
anything = \bytes -> { result: Err TooShort, rest: bytes }

comma : List U8 -> DecodeResult {}
comma = \bytes -> parseExactChar bytes ','

tryDecode : DecodeResult a, ({ val : a, rest : List U8 } -> DecodeResult b) -> DecodeResult b
tryDecode = \{ result, rest }, mapper ->
    when result is
        Ok val -> mapper { val, rest }
        Err e -> { result: Err e, rest }

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
takeJsonNumber : List U8 -> { taken : List U8, rest : List U8 }
takeJsonNumber = \bytes ->
    when List.walkUntil bytes Start numberHelp is
        Finish n | Zero n | Integer n | FractionB n | ExponentC n ->
            taken =
                bytes
                |> List.sublist { start: 0, len: n }
                |> List.dropIf \b -> b == '+'
                |> List.map \b -> if b == 'E' then 'e' else b

            { taken, rest: List.dropFirst bytes n }

        _ ->
            { taken: [], rest: bytes }

numberHelp : NumberState, U8 -> [Continue NumberState, Break NumberState]
numberHelp = \state, byte ->
    when (state, byte) is
        (Start, b) if b == '0' -> Continue (Zero 1)
        (Start, b) if b == '-' -> Continue (Minus 1)
        (Start, b) if isDigit1to9 b -> Continue (Integer 1)
        (Minus n, b) if b == '0' -> Continue (Zero (n + 1))
        (Minus n, b) if isDigit1to9 b -> Continue (Integer (n + 1))
        (Zero n, b) if b == '.' -> Continue (FractionA (n + 1))
        (Zero n, b) if isValidEnd b -> Break (Finish n)
        (Integer n, b) if isDigit0to9 b && n <= maxBytes -> Continue (Integer (n + 1))
        (Integer n, b) if b == '.' && n < maxBytes -> Continue (FractionA (n + 1))
        (Integer n, b) if isValidEnd b && n <= maxBytes -> Break (Finish n)
        (FractionA n, b) if isDigit0to9 b && n <= maxBytes -> Continue (FractionB (n + 1))
        (FractionB n, b) if isDigit0to9 b && n <= maxBytes -> Continue (FractionB (n + 1))
        (FractionB n, b) if b == 'e' || b == 'E' && n <= maxBytes -> Continue (ExponentA (n + 1))
        (FractionB n, b) if isValidEnd b && n <= maxBytes -> Break (Finish n)
        (ExponentA n, b) if b == '-' || b == '+' && n <= maxBytes -> Continue (ExponentB (n + 1))
        (ExponentA n, b) if isDigit0to9 b && n <= maxBytes -> Continue (ExponentC (n + 1))
        (ExponentB n, b) if isDigit0to9 b && n <= maxBytes -> Continue (ExponentC (n + 1))
        (ExponentC n, b) if isDigit0to9 b && n <= maxBytes -> Continue (ExponentC (n + 1))
        (ExponentC n, b) if isValidEnd b && n <= maxBytes -> Break (Finish n)
        _ -> Break Invalid

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
maxBytes : U64
maxBytes = 21 # Max bytes in a double precision float

isDigit0to9 : U8 -> Bool
isDigit0to9 = \b -> b >= '0' && b <= '9'

isDigit1to9 : U8 -> Bool
isDigit1to9 = \b -> b >= '1' && b <= '9'

isValidEnd : U8 -> Bool
isValidEnd = \b ->
    when b is
        ']' | ',' | ' ' | '\n' | '\r' | '\t' | '}' -> Bool.true
        _ -> Bool.false

expect
    actual = "0.0" |> Str.toUtf8 |> Decode.fromBytes json
    expected = Ok 0.0dec
    actual == expected

expect
    actual = "0" |> Str.toUtf8 |> Decode.fromBytes json
    expected = Ok 0u8
    actual == expected

expect
    actual = "1 " |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = { result: Ok 1dec, rest: [' '] }
    actual == expected

expect
    actual = "2]" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = { result: Ok 2u64, rest: [']'] }
    actual == expected

expect
    actual = "30,\n" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = { result: Ok 30i64, rest: [',', '\n'] }
    actual == expected

expect
    actual : DecodeResult U16
    actual = "+1" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = { result: Err TooShort, rest: ['+', '1'] }
    actual == expected

expect
    actual : DecodeResult U16
    actual = ".0" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = { result: Err TooShort, rest: ['.', '0'] }
    actual == expected

expect
    actual : DecodeResult U64
    actual = "-.1" |> Str.toUtf8 |> Decode.fromBytesPartial json
    actual.result == Err TooShort

expect
    actual : DecodeResult Dec
    actual = "72" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Ok 72dec
    actual.result == expected

expect
    actual : DecodeResult Dec
    actual = "-0" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Ok 0dec
    actual.result == expected

expect
    actual : DecodeResult Dec
    actual = "-7" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Ok -7dec
    actual.result == expected

expect
    actual : DecodeResult Dec
    actual = "-0\n" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = { result: Ok 0dec, rest: ['\n'] }
    actual == expected

expect
    actual : DecodeResult Dec
    actual = "123456789000 \n" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = { result: Ok 123456789000dec, rest: [' ', '\n'] }
    actual == expected

expect
    actual : DecodeResult Dec
    actual = "-12.03" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Ok -12.03
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "-12." |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "01.1" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = ".0" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "1.e1" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "-1.2E" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "0.1e+" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "-03" |> Str.toUtf8 |> Decode.fromBytesPartial json
    expected = Err TooShort
    actual.result == expected

# JSON STRING PRIMITIVE --------------------------------------------------------

# Decode a Json string primitive into a RocStr
#
# Note that decodeStr does not handle leading whitespace, any whitespace must be
# handled in json list or record decodin.
decodeString = Decode.custom \bytes, @Json {} ->
    when bytes is
        ['n', 'u', 'l', 'l', ..] ->
            { result: Ok "null", rest: List.dropFirst bytes 4 }

        _ ->
            { taken: strBytes, rest } = takeJsonString bytes

            if List.isEmpty strBytes then
                { result: Err TooShort, rest: bytes }
            else
                # Remove starting and ending quotation marks, replace unicode
                # escpapes with Roc equivalent, and try to parse RocStr from
                # bytes
                result =
                    strBytes
                    |> List.sublist {
                        start: 1,
                        len: Num.subSaturated (List.len strBytes) 2,
                    }
                    |> \bytesWithoutQuotationMarks ->
                        replaceEscapedChars { inBytes: bytesWithoutQuotationMarks, outBytes: [] }
                    |> .outBytes
                    |> Str.fromUtf8

                when result is
                    Ok str ->
                        { result: Ok str, rest }

                    Err _ ->
                        { result: Err TooShort, rest: bytes }

takeJsonString : List U8 -> { taken : List U8, rest : List U8 }
takeJsonString = \bytes ->
    when List.walkUntil bytes Start stringHelp is
        Finish n ->
            {
                taken: List.sublist bytes { start: 0, len: n },
                rest: List.dropFirst bytes n,
            }

        _ ->
            { taken: [], rest: bytes }

stringHelp : StringState, U8 -> [Continue StringState, Break StringState]
stringHelp = \state, byte ->
    when (state, byte) is
        (Start, b) if b == '"' -> Continue (Chars 1)
        (Chars n, b) if b == '"' -> Break (Finish (n + 1))
        (Chars n, b) if b == '\\' -> Continue (Escaped (n + 1))
        (Chars n, _) -> Continue (Chars (n + 1))
        (Escaped n, b) if isEscapedChar b -> Continue (Chars (n + 1))
        (Escaped n, b) if b == 'u' -> Continue (UnicodeA (n + 1))
        (UnicodeA n, b) if isHex b -> Continue (UnicodeB (n + 1))
        (UnicodeB n, b) if isHex b -> Continue (UnicodeC (n + 1))
        (UnicodeC n, b) if isHex b -> Continue (UnicodeD (n + 1))
        (UnicodeD n, b) if isHex b -> Continue (Chars (n + 1))
        _ -> Break (InvalidNumber)

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

isEscapedChar : U8 -> Bool
isEscapedChar = \b ->
    when b is
        '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' -> Bool.true
        _ -> Bool.false

escapedCharFromJson : U8 -> U8
escapedCharFromJson = \b ->
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

expect escapedCharFromJson 'n' == '\n'

isHex : U8 -> Bool
isHex = \b ->
    (b >= '0' && b <= '9')
    || (b >= 'a' && b <= 'f')
    || (b >= 'A' && b <= 'F')

expect isHex '0' && isHex 'f' && isHex 'F' && isHex 'A' && isHex '9'
expect !(isHex 'g' && isHex 'x' && isHex 'u' && isHex '\\' && isHex '-')

jsonHexToDecimal : U8 -> U8
jsonHexToDecimal = \b ->
    if b >= '0' && b <= '9' then
        b - '0'
    else if b >= 'a' && b <= 'f' then
        b - 'a' + 10
    else if b >= 'A' && b <= 'F' then
        b - 'A' + 10
    else
        crash "got an invalid hex char"

expect jsonHexToDecimal '0' == 0
expect jsonHexToDecimal '9' == 9
expect jsonHexToDecimal 'a' == 10
expect jsonHexToDecimal 'A' == 10
expect jsonHexToDecimal 'f' == 15
expect jsonHexToDecimal 'F' == 15

decimalHexToByte : U8, U8 -> U8
decimalHexToByte = \upper, lower ->
    Num.bitwiseOr (Num.shiftLeftBy upper 4) lower

expect
    actual = decimalHexToByte 3 7
    expected = '7'
    actual == expected

expect
    actual = decimalHexToByte 7 4
    expected = 't'
    actual == expected

hexToUtf8 : U8, U8, U8, U8 -> List U8
hexToUtf8 = \a, b, c, d ->
    i = jsonHexToDecimal a
    j = jsonHexToDecimal b
    k = jsonHexToDecimal c
    l = jsonHexToDecimal d

    if i == 0 && j == 0 then
        [decimalHexToByte k l]
    else
        [decimalHexToByte i j, decimalHexToByte k l]

# Test for \u0074 == U+74 == 't' in Basic Multilingual Plane
expect
    actual = hexToUtf8 '0' '0' '7' '4'
    expected = ['t']
    actual == expected

# Test for \u0068 == U+68 == 'h' in Basic Multilingual Plane
expect
    actual = hexToUtf8 '0' '0' '6' '8'
    expected = ['h']
    actual == expected

# Test for \u2c64 == U+2C64 == 'Ɽ' in Latin Extended-C
expect
    actual = hexToUtf8 '2' 'C' '6' '4'
    expected = [44, 100]
    actual == expected

unicodeReplacement = hexToUtf8 'f' 'f' 'd' 'd'

replaceEscapedChars : { inBytes : List U8, outBytes : List U8 } -> { inBytes : List U8, outBytes : List U8 }
replaceEscapedChars = \{ inBytes, outBytes } ->

    firstByte = List.get inBytes 0
    secondByte = List.get inBytes 1
    inBytesWithoutFirstTwo = List.dropFirst inBytes 2
    inBytesWithoutFirstSix = List.dropFirst inBytes 6

    when Pair firstByte secondByte is
        Pair (Ok a) (Ok b) if a == '\\' && b == 'u' ->
            # Extended json unicode escape
            when inBytesWithoutFirstTwo is
                [c, d, e, f, ..] ->
                    utf8Bytes = hexToUtf8 c d e f

                    replaceEscapedChars {
                        inBytes: inBytesWithoutFirstSix,
                        outBytes: List.concat outBytes utf8Bytes,
                    }

                _ ->
                    # Invalid Unicode Escape
                    replaceEscapedChars {
                        inBytes: inBytesWithoutFirstTwo,
                        outBytes: List.concat outBytes unicodeReplacement,
                    }

        Pair (Ok a) (Ok b) if a == '\\' && isEscapedChar b ->
            # Shorthand json unicode escape
            replaceEscapedChars {
                inBytes: inBytesWithoutFirstTwo,
                outBytes: List.append outBytes (escapedCharFromJson b),
            }

        Pair (Ok a) _ ->
            # Process next character
            replaceEscapedChars {
                inBytes: List.dropFirst inBytes 1,
                outBytes: List.append outBytes a,
            }

        _ ->
            { inBytes, outBytes }

# Test replacement of both extended and shorthand unicode escapes
expect
    inBytes = Str.toUtf8 "\\\\\\u0074\\u0068\\u0065\\t\\u0071\\u0075\\u0069\\u0063\\u006b\\n"
    actual = replaceEscapedChars { inBytes, outBytes: [] }
    expected = { inBytes: [], outBytes: ['\\', 't', 'h', 'e', '\t', 'q', 'u', 'i', 'c', 'k', '\n'] }

    actual == expected

# Test decode simple string
expect
    input = "\"hello\", " |> Str.toUtf8
    actual = Decode.fromBytesPartial input json
    expected = Ok "hello"

    actual.result == expected

# Test decode string with extended and shorthand json escapes
expect
    input = "\"h\\\"\\u0065llo\\n\"]\n" |> Str.toUtf8
    actual = Decode.fromBytesPartial input json
    expected = Ok "h\"ello\n"

    actual.result == expected

# Test json string decoding with escapes
expect
    input = Str.toUtf8 "\"a\r\nbc\\txz\"\t\n,  "
    actual = Decode.fromBytesPartial input json
    expected = Ok "a\r\nbc\txz"

    actual.result == expected

# Test decode of a null
expect
    input = Str.toUtf8 "null"
    actual = Decode.fromBytesPartial input json
    expected = Ok "null"

    actual.result == expected

# JSON ARRAYS ------------------------------------------------------------------

decodeList = \elemDecoder -> Decode.custom \bytes, @Json {} ->

        decodeElems = arrayElemDecoder elemDecoder

        result =
            when List.walkUntil bytes (BeforeOpeningBracket 0) arrayOpeningHelp is
                AfterOpeningBracket n -> Ok (List.dropFirst bytes n)
                _ -> Err ExpectedOpeningBracket

        when result is
            Ok elemBytes -> decodeElems elemBytes []
            Err ExpectedOpeningBracket ->
                crash "expected opening bracket"

arrayElemDecoder = \elemDecoder ->

    decodeElems = \bytes, accum ->

        # Done't need a comma before the first element
        state =
            if List.isEmpty accum then
                BeforeNextElement 0
            else
                BeforeNextElemOrClosingBracket 0

        when List.walkUntil bytes state arrayClosingHelp is
            AfterClosingBracket n ->
                # Eat remaining whitespace
                rest = List.dropFirst bytes n

                # Return List of decoded elements
                { result: Ok accum, rest }

            BeforeNextElement n ->
                # Eat any whitespace before element
                elemBytes = List.dropFirst bytes n

                # Decode current element
                { result, rest } = Decode.decodeWith elemBytes elemDecoder json

                when result is
                    Ok elem ->
                        # Accumulate decoded value and walk to next element
                        # or the end of the list
                        decodeElems rest (List.append accum elem)

                    Err _ ->
                        # Unable to decode next element
                        { result: Err TooShort, rest }

            BeforeNextElemOrClosingBracket _ ->
                if List.isEmpty accum then
                    # Handle empty lists
                    { result: Ok [], rest: bytes }
                else
                    # Expected comma or closing bracket after last element
                    { result: Err TooShort, rest: bytes }

    decodeElems

arrayOpeningHelp : ArrayOpeningState, U8 -> [Continue ArrayOpeningState, Break ArrayOpeningState]
arrayOpeningHelp = \state, byte ->
    when (state, byte) is
        (BeforeOpeningBracket n, b) if isWhitespace b -> Continue (BeforeOpeningBracket (n + 1))
        (BeforeOpeningBracket n, b) if b == '[' -> Continue (AfterOpeningBracket (n + 1))
        (AfterOpeningBracket n, b) if isWhitespace b -> Continue (AfterOpeningBracket (n + 1))
        _ -> Break state

arrayClosingHelp : ArrayClosingState, U8 -> [Continue ArrayClosingState, Break ArrayClosingState]
arrayClosingHelp = \state, byte ->
    when (state, byte) is
        (BeforeNextElemOrClosingBracket n, b) if isWhitespace b -> Continue (BeforeNextElemOrClosingBracket (n + 1))
        (BeforeNextElemOrClosingBracket n, b) if b == ',' -> Continue (BeforeNextElement (n + 1))
        (BeforeNextElemOrClosingBracket n, b) if b == ']' -> Continue (AfterClosingBracket (n + 1))
        (BeforeNextElement n, b) if isWhitespace b -> Continue (BeforeNextElement (n + 1))
        (BeforeNextElement n, b) if b == ']' -> Continue (AfterClosingBracket (n + 1))
        (AfterClosingBracket n, b) if isWhitespace b -> Continue (AfterClosingBracket (n + 1))
        _ -> Break state

isWhitespace = \b ->
    when b is
        ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false

expect
    input = ['1', 'a', ' ', '\n', 0x0d, 0x09]
    actual = List.map input isWhitespace
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
    input = Str.toUtf8 "[ ]"

    actual : DecodeResult (List U8)
    actual = Decode.fromBytesPartial input json

    actual.result == Ok []

# Test decode array of json numbers with whitespace
expect
    input = Str.toUtf8 "\n[\t 1 , 2  , 3]"

    actual : DecodeResult (List U64)
    actual = Decode.fromBytesPartial input json

    expected = Ok [1, 2, 3]

    actual.result == expected

# Test decode array of json strings ignoring whitespace
expect
    input = Str.toUtf8 "\n\t [\n \"one\"\r , \"two\" , \n\"3\"\t]"

    actual : DecodeResult (List Str)
    actual = Decode.fromBytesPartial input json
    expected = Ok ["one", "two", "3"]

    actual.result == expected

# JSON OBJECTS -----------------------------------------------------------------

decodeRecord = \initialState, stepField, finalizer -> Decode.custom \bytes, @Json {} ->

        # Recursively build up record from object field:value pairs
        decodeFields = \recordState, bytesBeforeField ->

            # Decode the json string field name
            { result: objectNameResult, rest: bytesAfterField } =
                Decode.decodeWith bytesBeforeField decodeString json

            # Count the bytes until the field value
            countBytesBeforeValue =
                when List.walkUntil bytesAfterField (BeforeColon 0) objectHelp is
                    AfterColon n -> n
                    _ -> 0

            valueBytes = List.dropFirst bytesAfterField countBytesBeforeValue

            when objectNameResult is
                Err TooShort ->
                    # Invalid object, unable to decode field name or find colon ':'
                    # after field and before the value
                    { result: Err TooShort, rest: bytes }

                Ok objectName ->
                    # Decode the json value
                    (
                        fieldName = objectName

                        # Retrieve value decoder for the current field
                        when stepField recordState fieldName is
                            Skip ->
                                # TODO This doesn't seem right, shouldn't we eat
                                # the remaining json object value bytes if we are skipping this
                                # field?
                                { result: Ok recordState, rest: valueBytes }

                            Keep valueDecoder ->
                                # Decode the value using the decoder from the recordState
                                # Note we need to pass json config options recursively here
                                Decode.decodeWith valueBytes valueDecoder (@Json {})
                    )
                    |> tryDecode \{ val: updatedRecord, rest: bytesAfterValue } ->
                        # Check if another field or '}' for end of object
                        when List.walkUntil bytesAfterValue (AfterObjectValue 0) objectHelp is
                            ObjectFieldNameStart n ->
                                rest = List.dropFirst bytesAfterValue n

                                # Decode the next field and value
                                decodeFields updatedRecord rest

                            AfterClosingBrace n ->
                                rest = List.dropFirst bytesAfterValue n

                                # Build final record from decoded fields and values
                                when finalizer updatedRecord json is
                                    Ok val -> { result: Ok val, rest }
                                    Err e -> { result: Err e, rest }

                            _ ->
                                # Invalid object
                                { result: Err TooShort, rest: bytesAfterValue }

        countBytesBeforeFirstField =
            when List.walkUntil bytes (BeforeOpeningBrace 0) objectHelp is
                ObjectFieldNameStart n -> n
                _ -> 0

        if countBytesBeforeFirstField == 0 then
            # Invalid object, expected opening brace '{' followed by a field
            { result: Err TooShort, rest: bytes }
        else
            bytesBeforeFirstField = List.dropFirst bytes countBytesBeforeFirstField

            # Begin decoding field:value pairs
            decodeFields initialState bytesBeforeFirstField

objectHelp : ObjectState, U8 -> [Break ObjectState, Continue ObjectState]
objectHelp = \state, byte ->
    when (state, byte) is
        (BeforeOpeningBrace n, b) if isWhitespace b -> Continue (BeforeOpeningBrace (n + 1))
        (BeforeOpeningBrace n, b) if b == '{' -> Continue (AfterOpeningBrace (n + 1))
        (AfterOpeningBrace n, b) if isWhitespace b -> Continue (AfterOpeningBrace (n + 1))
        (AfterOpeningBrace n, b) if b == '"' -> Break (ObjectFieldNameStart n)
        (BeforeColon n, b) if isWhitespace b -> Continue (BeforeColon (n + 1))
        (BeforeColon n, b) if b == ':' -> Continue (AfterColon (n + 1))
        (AfterColon n, b) if isWhitespace b -> Continue (AfterColon (n + 1))
        (AfterColon n, _) -> Break (AfterColon n)
        (AfterObjectValue n, b) if isWhitespace b -> Continue (AfterObjectValue (n + 1))
        (AfterObjectValue n, b) if b == ',' -> Continue (AfterComma (n + 1))
        (AfterObjectValue n, b) if b == '}' -> Continue (AfterClosingBrace (n + 1))
        (AfterComma n, b) if isWhitespace b -> Continue (AfterComma (n + 1))
        (AfterComma n, b) if b == '"' -> Break (ObjectFieldNameStart n)
        (AfterClosingBrace n, b) if isWhitespace b -> Continue (AfterClosingBrace (n + 1))
        (AfterClosingBrace n, _) -> Break (AfterClosingBrace n)
        _ -> Break InvalidObject

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
