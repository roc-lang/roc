interface Json
    exposes [
        Json,
        toUtf8,
        fromUtf8,
    ]
    imports [
        List,
        Str,
        Encode,
        Encode.{
            Encoder,
            EncoderFormatting,
            appendWith,
        },
        Decode,
        Decode.{
            DecoderFormatting,
            DecodeResult,
        },
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
            F32,
            F64,
            Dec,
        },
        Bool.{ Bool, Eq },
        Result,
    ]

## **Note:** This module is likely to be moved out of the builtins in future.
## It is currently located here to facilitate development of the Abilities
## language feature and testing. You are welcome to use this module, just note
## that it will be moved into a package in a future update.
Json := {} has [
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
         },
     ]

toUtf8 = @Json {}

fromUtf8 = @Json {}

numToBytes = \n ->
    n |> Num.toStr |> Str.toUtf8

encodeU8 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeU16 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeU32 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeU64 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeU128 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI8 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI16 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI32 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI64 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeI128 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeF32 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeF64 = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeDec = \n -> Encode.custom \bytes, @Json {} -> List.concat bytes (numToBytes n)

encodeBool = \b -> Encode.custom \bytes, @Json {} ->
        if
            b
        then
            List.concat bytes (Str.toUtf8 "true")
        else
            List.concat bytes (Str.toUtf8 "false")

encodeString = \s -> Encode.custom \bytes, @Json {} ->
        List.append bytes (Num.toU8 '"')
        |> List.concat (Str.toUtf8 s)
        |> List.append (Num.toU8 '"')

encodeList = \lst, encodeElem ->
    Encode.custom \bytes, @Json {} ->
        writeList = \{ buffer, elemsLeft }, elem ->
            bufferWithElem = appendWith buffer (encodeElem elem) (@Json {})
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
            bufferWithKeyValue =
                List.append buffer (Num.toU8 '"')
                |> List.concat (Str.toUtf8 key)
                |> List.append (Num.toU8 '"')
                |> List.append (Num.toU8 ':')
                |> appendWith value (@Json {})

            bufferWithSuffix =
                if fieldsLeft > 1 then
                    List.append bufferWithKeyValue (Num.toU8 ',')
                else
                    bufferWithKeyValue

            { buffer: bufferWithSuffix, fieldsLeft: fieldsLeft - 1 }

        bytesHead = List.append bytes (Num.toU8 '{')
        { buffer: bytesWithRecord } = List.walk fields { buffer: bytesHead, fieldsLeft: List.len fields } writeRecord

        List.append bytesWithRecord (Num.toU8 '}')

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

isEscapeSequence : U8, U8 -> Bool
isEscapeSequence = \a, b ->
    when P a b is
        P '\\' 'b' -> Bool.true # Backspace
        P '\\' 'f' -> Bool.true # Form feed
        P '\\' 'n' -> Bool.true # Newline
        P '\\' 'r' -> Bool.true # Carriage return
        P '\\' 't' -> Bool.true # Tab
        P '\\' '"' -> Bool.true # Double quote
        P '\\' '\\' -> Bool.true # Backslash
        _ -> Bool.false

takeWhile = \list, predicate ->
    helper = \{ taken, rest } ->
        when rest is
            [a, b, ..] ->
                if isEscapeSequence a b then
                    helper {
                        taken: taken |> List.append a |> List.append b,
                        rest: List.drop rest 2,
                    }
                else if predicate a then
                    helper {
                        taken: List.append taken a,
                        rest: List.dropFirst rest,
                    }
                else
                    { taken, rest }

            [a, ..] if predicate a ->
                helper {
                    taken: List.append taken a,
                    rest: List.dropFirst rest,
                }

            _ -> { taken, rest }

    helper { taken: [], rest: list }

digits : List U8
digits = List.range { start: At '0', end: At '9' }

takeDigits = \bytes ->
    takeWhile bytes \n -> List.contains digits n

takeFloat = \bytes ->
    { taken: intPart, rest } = takeDigits bytes

    when List.get rest 0 is
        Ok '.' ->
            { taken: floatPart, rest: afterAll } = takeDigits (List.split rest 1).others
            builtFloat =
                List.concat (List.append intPart '.') floatPart

            { taken: builtFloat, rest: afterAll }

        _ ->
            { taken: intPart, rest }

decodeU8 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU8 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeU16 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU16 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeU32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU32 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeU64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU64 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeU128 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toU128 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI8 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI8 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI16 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI16 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI32 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI64 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeI128 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeDigits bytes

    when Str.fromUtf8 taken |> Result.try Str.toI128 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeF32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeFloat bytes

    when Str.fromUtf8 taken |> Result.try Str.toF32 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeF64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeFloat bytes

    when Str.fromUtf8 taken |> Result.try Str.toF64 is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeDec = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeFloat bytes

    when Str.fromUtf8 taken |> Result.try Str.toDec is
        Ok n -> { result: Ok n, rest }
        Err _ -> { result: Err TooShort, rest }

decodeBool = Decode.custom \bytes, @Json {} ->
    { before: maybeFalse, others: afterFalse } = List.split bytes 5

    # Note: this could be more performant by traversing both branches char-by-char.
    # Doing that would also make `rest` more correct in the erroring case.
    if
        maybeFalse == ['f', 'a', 'l', 's', 'e']
    then
        { result: Ok Bool.false, rest: afterFalse }
    else
        { before: maybeTrue, others: afterTrue } = List.split bytes 4

        if
            maybeTrue == ['t', 'r', 'u', 'e']
        then
            { result: Ok Bool.true, rest: afterTrue }
        else
            { result: Err TooShort, rest: bytes }

jsonString : List U8 -> DecodeResult Str
jsonString = \bytes ->
    { before, others: afterStartingQuote } = List.split bytes 1

    if
        before == ['"']
    then
        { taken: strSequence, rest } = takeWhile afterStartingQuote \n -> n != '"'

        when Str.fromUtf8 strSequence is
            Ok s ->
                { others: afterEndingQuote } = List.split rest 1

                { result: Ok s, rest: afterEndingQuote }

            Err _ -> { result: Err TooShort, rest }
    else
        { result: Err TooShort, rest: bytes }

decodeString = Decode.custom \bytes, @Json {} ->
    jsonString bytes

decodeList = \decodeElem -> Decode.custom \bytes, @Json {} ->

        decodeElems = \chunk, accum ->
            when Decode.decodeWith chunk decodeElem (@Json {}) is
                { result, rest } ->
                    when result is
                        Err e -> Errored e rest
                        Ok val ->
                            restWithoutWhitespace = eatWhitespace rest
                            when restWithoutWhitespace is
                                [',', ..] -> decodeElems (eatWhitespace (List.dropFirst restWithoutWhitespace)) (List.append accum val)
                                _ -> Done (List.append accum val) restWithoutWhitespace

        when bytes is
            ['[', ']'] -> { result: Ok [], rest: List.drop bytes 2 }
            ['[', ..] ->
                when decodeElems (eatWhitespace (List.dropFirst bytes)) [] is
                    Errored e rest -> { result: Err e, rest }
                    Done vals rest ->
                        when rest is
                            [']', ..] -> { result: Ok vals, rest: List.dropFirst rest }
                            _ -> { result: Err TooShort, rest }

            _ ->
                { result: Err TooShort, rest: bytes }

parseExactChar : List U8, U8 -> DecodeResult {}
parseExactChar = \bytes, char ->
    when List.get bytes 0 is
        Ok c ->
            if
                c == char
            then
                { result: Ok {}, rest: (List.split bytes 1).others }
            else
                { result: Err TooShort, rest: bytes }

        Err _ -> { result: Err TooShort, rest: bytes }

openBrace : List U8 -> DecodeResult {}
openBrace = \bytes -> parseExactChar bytes '{'

closingBrace : List U8 -> DecodeResult {}
closingBrace = \bytes -> parseExactChar bytes '}'

recordKey : List U8 -> DecodeResult Str
recordKey = \bytes -> jsonString bytes

anything : List U8 -> DecodeResult {}
anything = \bytes -> { result: Err TooShort, rest: bytes }

colon : List U8 -> DecodeResult {}
colon = \bytes -> parseExactChar bytes ':'

comma : List U8 -> DecodeResult {}
comma = \bytes -> parseExactChar bytes ','

tryDecode : DecodeResult a, ({ val : a, rest : List U8 } -> DecodeResult b) -> DecodeResult b
tryDecode = \{ result, rest }, mapper ->
    when result is
        Ok val -> mapper { val, rest }
        Err e -> { result: Err e, rest }

decodeRecord = \initialState, stepField, finalizer -> Decode.custom \bytes, @Json {} ->
        # NB: the stepper function must be passed explicitly until #2894 is resolved.
        decodeFields = \stepper, state, kvBytes ->
            { val: key, rest } <- recordKey kvBytes |> tryDecode
            { rest: afterColonBytes } <- colon rest |> tryDecode
            { val: newState, rest: beforeCommaOrBreak } <- tryDecode
                    (
                        when stepper state key is
                            Skip ->
                                { rest: beforeCommaOrBreak } <- afterColonBytes |> anything |> tryDecode
                                { result: Ok state, rest: beforeCommaOrBreak }

                            Keep decoder ->
                                Decode.decodeWith afterColonBytes decoder (@Json {})
                    )

            { result: commaResult, rest: nextBytes } = comma beforeCommaOrBreak

            when commaResult is
                Ok {} -> decodeFields stepField newState nextBytes
                Err _ -> { result: Ok newState, rest: nextBytes }

        { rest: afterBraceBytes } <- bytes |> openBrace |> tryDecode

        { val: endStateResult, rest: beforeClosingBraceBytes } <- decodeFields stepField initialState afterBraceBytes |> tryDecode

        { rest: afterRecordBytes } <- beforeClosingBraceBytes |> closingBrace |> tryDecode

        when finalizer endStateResult is
            Ok val -> { result: Ok val, rest: afterRecordBytes }
            Err e -> { result: Err e, rest: afterRecordBytes }

# Helper to eat leading Json whitespace characters
eatWhitespace = \input ->
    when input is
        [' ', ..] -> eatWhitespace (List.dropFirst input)
        ['\n', ..] -> eatWhitespace (List.dropFirst input)
        ['\r', ..] -> eatWhitespace (List.dropFirst input)
        ['\t', ..] -> eatWhitespace (List.dropFirst input)
        _ -> input

# Test eating Json whitespace
expect
    input = Str.toUtf8 " \n\r\tabc"
    actual = eatWhitespace input
    expected = Str.toUtf8 "abc"
    actual == expected

# Test json string decoding with escapes
expect
    input = Str.toUtf8 "\"a\r\nbc\\\"xz\""
    expected = Ok "a\r\nbc\\\"xz"
    actual = Decode.fromBytes input fromUtf8
    actual == expected

# Test json string encoding with escapes
expect
    input = "a\r\nbc\\\"xz"
    expected = Str.toUtf8 "\"a\r\nbc\\\"xz\""
    actual = Encode.toBytes input toUtf8
    actual == expected

# Test json array decode empty list
expect
    input = Str.toUtf8 "[ ]"
    expected = []

    actual : List U8
    actual = Decode.fromBytes input fromUtf8 |> Result.withDefault []

    actual == expected

# Test json array decoding into integers
expect
    input = Str.toUtf8 "[ 1,\n2,\t3]"
    expected = [1, 2, 3]

    actual : List U8
    actual = Decode.fromBytes input fromUtf8 |> Result.withDefault []

    actual == expected

# Test json array decoding into strings ignoring whitespace around values
expect
    input = Str.toUtf8 "[\r\"one\" ,\t\"two\"\n,\n\"3\"\t]"
    expected = ["one", "two", "3"]

    actual : List Str
    actual =
        Decode.fromBytes input fromUtf8
        |> Result.onErr handleJsonDecodeError
        |> Result.withDefault []

    actual == expected

# Helper for tests to handle Json decoding errors
handleJsonDecodeError = \err ->
    when err is
        Leftover bytes ->
            when Str.fromUtf8 bytes is
                Ok bs -> crash "ERROR: bytes left \(bs)"
                Err _ ->
                    ls =
                        bytes
                        |> List.map Num.toStr
                        |> Str.joinWith ","

                    crash "ERROR: bytes left \(ls)"

        TooShort -> crash "ERROR: input too short"
