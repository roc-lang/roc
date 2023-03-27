## JSON is a data format that is easy for humans to read and write. It is
## commonly used to exhange data between two systems such as a server and a
## client (e.g. web browser).
##
## This module implements functionality to serialise and de-serialise Roc types
## to and from JSON data. Using the `Encode` and `Decode` builtins this process
## can be achieved without the need to write custom encoder and decoder functions
## to parse UTF-8 strings.
##
## Here is a basic example which shows how to parse a JSON record into a Roc
## type named `Language` which includes a `name` field. The JSON string is
## decoded and then the field is encoded back into a UTF-8 string.
##
## ```
## Language : {
##     name : Str,
## }
##
## jsonStr = Str.toUtf8 "{\"name\":\"Röc Lang\"}"
##
## result : Result Language _
## result =
##     jsonStr
##     |> Decode.fromBytes fromUtf8 # returns `Ok {name : "Röc Lang"}`
##
## name =
##     decodedValue <- Result.map result
##
##     Encode.toBytes decodedValue.name toUtf8
##
## expect name == Ok (Str.toUtf8 "\"Röc Lang\"")
## ```
##
## **Note:** This module is likely to be moved out of the builtins in future.
## It is currently located here to facilitate development of the Abilities
## language feature and testing. You are welcome to use this module, just note
## that it will be moved into a package in a future update.
interface Json
    exposes [
        Json,
        toUtf8,
        fromUtf8,
    ]
    imports [
        List,
        Str,
        Result.{ Result },
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
            Nat,
            Dec,
        },
        Bool.{ Bool, Eq },
        Result,
    ]

## An opaque type with the `EncoderFormatting` and
## `DecoderFormatting` abilities.
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

## Returns a JSON `Decoder`
toUtf8 = @Json {}

## Returns a JSON `Encoder`
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

encodeTuple = \elems ->
    Encode.custom \bytes, @Json {} ->
        writeTuple = \{ buffer, elemsLeft }, elemEncoder ->
            bufferWithElem =
                appendWith buffer elemEncoder (@Json {})

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

decodeU16 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU16
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeU32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU32
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeU64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU64
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeU128 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU128
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeI8 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI8
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeI16 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI16
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeI32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI32
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeI64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI64
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeI128 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI128
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeF32 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toF32
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeF64 = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toF64
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeDec = Decode.custom \bytes, @Json {} ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toDec
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeBool = Decode.custom \bytes, @Json {} ->
    when bytes is
        ['f', 'a', 'l', 's', 'e', ..] -> { result: Ok Bool.false, rest: List.drop bytes 5 }
        ['t', 'r', 'u', 'e', ..] -> { result: Ok Bool.true, rest: List.drop bytes 4 }
        _ -> { result: Err TooShort, rest: bytes }

expect
    actual : DecodeResult Bool
    actual = "true\n" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Ok Bool.true
    actual.result == expected

expect
    actual : DecodeResult Bool
    actual = "false ]\n" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Ok Bool.false
    actual.result == expected

decodeTuple = \initialState, stepElem, finalizer -> Decode.custom \initialBytes, @Json {} ->
        # NB: the stepper function must be passed explicitly until #2894 is resolved.
        decodeElems = \stepper, state, index, bytes ->
            { val: newState, rest: beforeCommaOrBreak } <- tryDecode
                    (
                        when stepper state index is
                            TooLong ->
                                { rest: beforeCommaOrBreak } <- bytes |> anything |> tryDecode
                                { result: Ok state, rest: beforeCommaOrBreak }

                            Next decoder ->
                                Decode.decodeWith bytes decoder (@Json {})
                    )

            { result: commaResult, rest: nextBytes } = comma beforeCommaOrBreak

            when commaResult is
                Ok {} -> decodeElems stepElem newState (index + 1) nextBytes
                Err _ -> { result: Ok newState, rest: nextBytes }

        { rest: afterBracketBytes } <- initialBytes |> openBracket |> tryDecode

        { val: endStateResult, rest: beforeClosingBracketBytes } <- decodeElems stepElem initialState 0 afterBracketBytes |> tryDecode

        { rest: afterTupleBytes } <- beforeClosingBracketBytes |> closingBracket |> tryDecode

        when finalizer endStateResult is
            Ok val -> { result: Ok val, rest: afterTupleBytes }
            Err e -> { result: Err e, rest: afterTupleBytes }

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

openBracket : List U8 -> DecodeResult {}
openBracket = \bytes -> parseExactChar bytes '['

closingBracket : List U8 -> DecodeResult {}
closingBracket = \bytes -> parseExactChar bytes ']'

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

            { taken, rest: List.drop bytes n }

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
    Minus Nat,
    Zero Nat,
    Integer Nat,
    FractionA Nat,
    FractionB Nat,
    ExponentA Nat,
    ExponentB Nat,
    ExponentC Nat,
    Invalid,
    Finish Nat,
]

maxBytes : Nat
maxBytes = 21 # Max bytes in a double precision float

isDigit0to9 : U8 -> Bool
isDigit0to9 = \b -> b >= '0' && b <= '9'

isDigit1to9 : U8 -> Bool
isDigit1to9 = \b -> b >= '1' && b <= '9'

isValidEnd : U8 -> Bool
isValidEnd = \b ->
    when b is
        ']' | ',' | ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false

expect
    actual = "0.0" |> Str.toUtf8 |> Decode.fromBytes fromUtf8
    expected = Ok 0.0dec
    actual == expected

expect
    actual = "0" |> Str.toUtf8 |> Decode.fromBytes fromUtf8
    expected = Ok 0u8
    actual == expected

expect
    actual = "1 " |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = { result: Ok 1dec, rest: [' '] }
    actual == expected

expect
    actual = "2]" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = { result: Ok 2u64, rest: [']'] }
    actual == expected

expect
    actual = "30,\n" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = { result: Ok 30i64, rest: [',', '\n'] }
    actual == expected

expect
    actual : DecodeResult U16
    actual = "+1" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = { result: Err TooShort, rest: ['+', '1'] }
    actual == expected

expect
    actual : DecodeResult U16
    actual = ".0" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = { result: Err TooShort, rest: ['.', '0'] }
    actual == expected

expect
    actual : DecodeResult U64
    actual = "-.1" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    actual.result == Err TooShort

expect
    actual : DecodeResult Dec
    actual = "72" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Ok 72dec
    actual.result == expected

expect
    actual : DecodeResult Dec
    actual = "-0" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Ok 0dec
    actual.result == expected

expect
    actual : DecodeResult Dec
    actual = "-7" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Ok -7dec
    actual.result == expected

expect
    actual : DecodeResult Dec
    actual = "-0\n" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = { result: Ok 0dec, rest: ['\n'] }
    actual == expected

expect
    actual : DecodeResult Dec
    actual = "123456789000 \n" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = { result: Ok 123456789000dec, rest: [' ', '\n'] }
    actual == expected

expect
    actual : DecodeResult Dec
    actual = "-12.03" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Ok -12.03
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "-12." |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "01.1" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = ".0" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "1.e1" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "-1.2E" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "0.1e+" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Err TooShort
    actual.result == expected

expect
    actual : DecodeResult U64
    actual = "-03" |> Str.toUtf8 |> Decode.fromBytesPartial fromUtf8
    expected = Err TooShort
    actual.result == expected

# JSON STRING PRIMITIVE --------------------------------------------------------

# TODO add support for 'null' decoding

# Decode a Json string primitive into a RocStr
#
# Note that decodeStr does not handle leading whitespace, any whitespace must be
# handled in json list or record decodin.
decodeString = Decode.custom \bytes, @Json {} ->
    { taken: strBytes, rest } = bytes |> takeJsonString

    if List.isEmpty strBytes then
        { result: Err TooShort, rest: bytes }
    else
        # Replace unicode escpapes with Roc equivalent
        { outBytes: strBytesReplaced } =
            replaceEscapedChars { inBytes: strBytes, outBytes: [] }

        # Try to parse RocStr from bytes
        result =
            strBytesReplaced
            |> List.dropFirst # Remove starting quotation mark
            |> List.dropLast # Remove ending quotation mark
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
                rest: List.drop bytes n,
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
        _ -> Break (Invalid)

StringState : [
    Start,
    Chars Nat,
    Escaped Nat,
    UnicodeA Nat,
    UnicodeB Nat,
    UnicodeC Nat,
    UnicodeD Nat,
    Finish Nat,
    Invalid,
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
    inBytesWithoutFirstTwo = List.drop inBytes 2
    inBytesWithoutFirstSix = List.drop inBytes 6

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
                inBytes: List.dropFirst inBytes,
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
    actual = Decode.fromBytesPartial input fromUtf8
    expected = Ok "hello"

    actual.result == expected

# Test decode string with extended and shorthand json escapes
expect
    input = "\"h\\\"\\u0065llo\\n\"]\n" |> Str.toUtf8
    actual = Decode.fromBytesPartial input fromUtf8
    expected = Ok "h\"ello\n"

    actual.result == expected

# Test json string decoding with escapes
expect
    input = Str.toUtf8 "\"a\r\nbc\\txz\"\t\n,  "
    actual = Decode.fromBytesPartial input fromUtf8
    expected = Ok "a\r\nbc\txz"

    actual.result == expected

# TODO fix encoding of escapes, this test is not compliant with the spec
# Test json string encoding with escapes
# e.g. "\r" encodes to "\\r" or "\\u000D" as Carriage Return is U+000D
# expect
#     input = "a\r\nbc\\\"xz"
#     expected = Str.toUtf8 "\"a\r\nbc\\\"xz\""
#     actual = Encode.toBytes input toUtf8

#     actual == expected

# JSON ARRAYS ------------------------------------------------------------------

decodeList = \elemDecoder -> Decode.custom \bytes, @Json {} ->

        decodeElems = arrayElemDecoder elemDecoder

        result =
            when List.walkUntil bytes (BeforeOpeningBracket 0) arrayOpeningHelp is
                AfterOpeningBracket n -> Ok (List.drop bytes n)
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
                rest = List.drop bytes n

                # Return List of decoded elements
                { result: Ok accum, rest }

            BeforeNextElement n ->
                # Eat any whitespace before element
                elemBytes = List.drop bytes n

                # Decode current element
                { result, rest } = Decode.decodeWith elemBytes elemDecoder fromUtf8

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
    BeforeOpeningBracket Nat,
    AfterOpeningBracket Nat,
]

ArrayClosingState : [
    BeforeNextElemOrClosingBracket Nat,
    BeforeNextElement Nat,
    AfterClosingBracket Nat,
]

# Test decoding an empty array
expect
    input = Str.toUtf8 "[ ]"

    actual : DecodeResult (List U8)
    actual = Decode.fromBytesPartial input fromUtf8

    actual.result == Ok []

# Test decode array of json numbers with whitespace
expect
    input = Str.toUtf8 "\n[\t 1 , 2  , 3]"

    actual : DecodeResult (List U64)
    actual = Decode.fromBytesPartial input fromUtf8

    expected = Ok [1, 2, 3]

    actual.result == expected

# Test decode array of json strings ignoring whitespace
expect
    input = Str.toUtf8 "\n\t [\n \"one\"\r , \"two\" , \n\"3\"\t]"

    actual : DecodeResult (List Str)
    actual = Decode.fromBytesPartial input fromUtf8
    expected = Ok ["one", "two", "3"]

    actual.result == expected

# JSON OBJECTS -----------------------------------------------------------------

decodeRecord = \initialState, stepField, finalizer -> Decode.custom \bytes, @Json {} ->
        # NB: the stepper function must be passed explicitly until #2894 is resolved.
        decodeFields = \stepper, state, kvBytes ->
            { val: key, rest } <- (Decode.decodeWith kvBytes decodeString (@Json {})) |> tryDecode
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

# # Test decode of simple Json Object into Roc Record
# expect
#     input = Str.toUtf8 "{\"fruit\":2}"

#     actual = Decode.fromBytesPartial input fromUtf8
#     expected = Ok { fruit: 2 }

#     actual.result == expected
