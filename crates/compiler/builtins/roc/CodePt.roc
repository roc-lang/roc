interface CodePt
    exposes [CodePt, utf8Len, toU32, fromU32, isHighSurrogate, isLowSurrogate, isValidScalar]
    imports [CodePtInternal.{ fromU32Unchecked }]

## A [Unicode code point](http://www.unicode.org/glossary/#code_point).
CodePt : CodePtInternal.CodePtInternal

## Converts a [CodePt] to its underlying [Unicode code point](http://www.unicode.org/glossary/#code_point)
## integer representation.
toU32 : CodePt -> U32
toU32 = CodePtInternal.toU32

## Converts a [U32] to a [CodePt] by verifying that it is a valid [Unicode code point](http://www.unicode.org/glossary/#code_point)
## (that is, it's between `0` and `0x10FFFF`).
fromU32 : U32 -> Result CodePt [InvalidCodePt]
fromU32 = \u32 ->
    # Definition: http://www.unicode.org/glossary/#code_point
    if u32 <= 0x10FFFF then
        Ok (fromU32Unchecked u32)
    else
        Err InvalidCodePt


## Returns false if this is either a [high-surrogate code point](http://www.unicode.org/glossary/#high_surrogate_code_point)
## or a [low-surrogate code point](http://www.unicode.org/glossary/#high_surrogate_code_point).
##
## To check for either of those individually, use [isHighSurrogate] or [isLowSurrogate]
isValidScalar : CodePt -> Bool
isValidScalar = \codePt -> !(isHighSurrogate codePt) && !(isLowSurrogate codePt)

## Returns true if this is a [high-surrogate code point](http://www.unicode.org/glossary/#high_surrogate_code_point)
## (`0xD800` to `0xDBFF`)
isHighSurrogate : CodePt -> Bool
isHighSurrogate = \codePt ->
    u32 = CodePtInternal.toU32 codePt

    u32 >= 0xDC00 && u32 <= 0xDFFF

## Returns true if this is a [low-surrogate code point](http://www.unicode.org/glossary/#high_surrogate_code_point)
## (`0xD800` to `0xDBFF`)
isLowSurrogate : CodePt -> Bool
isLowSurrogate = \codePt ->
    u32 = CodePtInternal.toU32 codePt

    u32 >= 0xDC00 && u32 <= 0xDFFF

## Zig docs: bytes the UTF-8 representation would require
## for the given codepoint.
utf8Len : CodePt -> Result Nat [InvalidCodePt]
utf8Len = \codePt ->
    u32 = CodePtInternal.toU32 codePt

    if u32 < 0x80 then
        Ok 1
    else if u32 < 0x800 then
        Ok 2
    else if u32 < 0x10000 then
        Ok 3
    else if u32 < 0x110000 then
        Ok 4
    else
        Err InvalidCodePt

## Encode a [Scalar] as UTF-8 bytes and append those bytes to an existing list of UTF-8 bytes.
appendToUtf8 : List U8, CodePt -> List U8
appendToUtf8 = \bytes, codePoint ->
    codePt = CodePtInternal.toU32 codePoint

    if codePt < 0x80 then
        List.append bytes (Num.toU8 codePt)
    else if codePt < 0x800 then
        byte1 =
            codePt
            |> Num.shiftRightBy 6
            |> Num.bitwiseOr 0b11000000
            |> Num.toU8

        byte2 =
            codePt
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        List.append2 bytes byte1 byte2
    else if codePt < 0x10000 then
        byte1 =
            codePt
            |> Num.shiftRightBy 12
            |> Num.bitwiseOr 0b11100000
            |> Num.toU8

        byte2 =
            codePt
            |> Num.shiftRightBy 6
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        byte3 =
            codePt
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        List.append3 bytes byte1 byte2 byte3
    else
        ## This was an invalid Unicode scalar value, even though it had the Roc type Scalar.
        ## This should never happen!
        expect codePt < 0x110000

        byte1 =
            codePt
            |> Num.shiftRightBy 18
            |> Num.bitwiseOr 0b11110000
            |> Num.toU8

        byte2 =
            codePt
            |> Num.shiftRightBy 12
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        byte3 =
            codePt
            |> Num.shiftRightBy 6
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        byte4 =
            codePt
            |> Num.bitwiseAnd 0b111111
            |> Num.bitwiseOr 0b10000000
            |> Num.toU8

        List.append4 bytes byte1 byte2 byte3 byte4

## The number of UTF-8 bytes it takes to represent this [Scalar].
countUtf8Bytes : CodePt -> Nat
countUtf8Bytes = \codePoint ->
    codePt = CodePtInternal.toU32 codePoint

    if codePt < 0x80 then
        1
    else if codePt < 0x800 then
        2
    else if codePt < 0x10000 then
        3
    else
        # If this expectation fails, it was an invalid Scalar and shouldn't have been allowed!
        expect codePt < 0x110000
        4

## parse a 2-byte code point
parse2 : U8, U8 -> Result U32 [InvalidUtf8]
parse2 = \firstByte, secondByte ->
    crash "TODO"

## parse a 3-byte code point
parse3 : U8, U8, U8 -> Result U32 [InvalidUtf8]
parse3 = \first, second, third ->
    crash "TODO"

## parse a 4-byte code point
parse4 : U8, U8, U8, U8 -> Result U32 [InvalidUtf8]
parse4 = \first, second, third ->
    crash "TODO"

## Parses the first code point found in a list of bytes encoded as UTF-8. Returns `ListWasEmpty`
## if the list was empty, or `InvalidUtf8` if the bytes were not valid UTF-8.
parseUtf8 : List U8 -> Result { codePt : CodePt, bytesParsed : Nat } [InvalidUtf8, ListWasEmpty]
parseUtf8 = \bytes ->
    # We always try to get the first byte, and if it fails with Err ListWasEmpty, then
    # the whole function should return Err ListWasEmpty. This tells the caller "there's nothing
    # else to parse" as opposed to "There are bytes here, but they're invalid UTF-8."
    firstByte <- List.first bytes |> Result.try

    # Get the byte at the index, or return Err InvalidUtf8 if that's past the end of the list.
    byteAt = \index ->
        List.get bytes index
        |> Result.mapErr \OutOfBounds -> InvalidUtf8

    # Some of the bits in the first byte tell us the total byte length of the code point.
    if firstByte <= 0b0111_1111 then
        # 1-byte code point
        bytesParsed = 1

        Ok {
            codePt: fromU32Unchecked (Num.toU32 firstByte),
            bytesParsed,
        }
    else if firstByte >= 0b1100_0000 && firstByte <= 0b1101_1111 then
        # 2-byte code point
        secondByte <- byteAt 1 |> Result.try
        bytesParsed = 2

        parse2 firstByte secondByte
        |> Result.map \u32 -> { codePt: fromU32Unchecked u32, bytesParsed }
    else if firstByte >= 0b1110_0000 && firstByte <= 0b1110_1111 then
        # 3-byte code point
        secondByte <- byteAt 1 |> Result.try
        thirdByte <- byteAt 2 |> Result.try
        bytesParsed = 3

        if
            Num.bitwiseAnd firstByte 0b11110000 == 0b11100000
            && Num.bitwiseAnd secondByte 0b11000000 == 0b10000000
            && Num.bitwiseAnd thirdByte 0b11000000 == 0b10000000
        then
            parse3 firstByte secondByte thirdByte
            |> Result.map \u32 -> { codePt: fromU32Unchecked u32, bytesParsed }
        else
            Err InvalidUtf8
    else if firstByte >= 0b1111_0000 && firstByte <= 0b1111_0111 then
        # 4-byte code point
        secondByte <- byteAt 1 |> Result.try
        thirdByte <- byteAt 2 |> Result.try
        fourthByte <- byteAt 3 |> Result.try
        bytesParsed = 4

        if
            Num.bitwiseAnd firstByte 0b11111000 == 0b11110000
            && Num.bitwiseAnd secondByte 0b11000000 == 0b10000000
            && Num.bitwiseAnd thirdByte 0b11000000 == 0b10000000
            && Num.bitwiseAnd fourthByte 0b11000000 == 0b10000000
        then
            parse4 firstByte secondByte thirdByte fourthByte
            |> Result.map \u32 -> { codePt: fromU32Unchecked u32, bytesParsed }
        else
            Err InvalidUtf8
    else
        Err InvalidUtf8

## Empty input
expect [] |> parseUtf8 == Err ListWasEmpty

## Incorrect continuation byte
expect [0xC3, 0x28] |> parseUtf8 == Err InvalidUtf8

## Overlong encoding for ASCII character
expect [0xC0, 0xA1] |> parseUtf8 == Err InvalidUtf8

## Overlong encoding for 2-byte character
expect [0xE0, 0x80, 0xAF] |> parseUtf8 == Err InvalidUtf8

## Overlong encoding for 3-byte character
expect [0xF0, 0x80, 0x80, 0xA6] |> parseUtf8 == Err InvalidUtf8

## Invalid four-byte encoding (scalar value too large)
expect [0xF4, 0x90, 0x80, 0x80] |> parseUtf8 == Err InvalidUtf8

## Invalid first byte (>= 0xF8)
expect [0xF8, 0xA1, 0xA2, 0xA3, 0xA4] |> parseUtf8 == Err InvalidUtf8

## Invalid first byte (>= 0xC0 and <= 0xC1)
expect [0xC0, 0x80] |> parseUtf8 == Err InvalidUtf8

## Multiple valid 1-byte scalars
expect "hello" |> Str.toUtf8 |> parseUtf8 == Ok { codePt: fromU32Unchecked 'h', bytesParsed: 1 }

## Multiple valid 3-byte scalars
expect "世界" |> Str.toUtf8 |> parseUtf8 == Ok { codePt: fromU32Unchecked '世', bytesParsed: 3 }

## Valid 1-byte followed by valid multi-byte scalar
expect "lé" |> Str.toUtf8 |> parseUtf8 == Ok { codePt: fromU32Unchecked 'l', bytesParsed: 1 }

## Valid 2-byte character followed by multiple single-byte scalar
expect "élan" |> Str.toUtf8 |> parseUtf8 == Ok { codePt: fromU32Unchecked 'é', bytesParsed: 2 }

## Valid 3-byte followed by valid single-byte scalar
expect "界s" |> Str.toUtf8 |> parseUtf8 == Ok { codePt: fromU32Unchecked '界', bytesParsed: 3 }

## Valid UTF-8 followed by invalid UTF-8 (stops before invalid UTF-8)
expect (List.concat (Str.toUtf8 "h") [0xC0, 0x80]) |> parseUtf8 == Ok { codePt: fromU32Unchecked 'h', bytesParsed: 1 }

## Valid 3-byte followed by invalid UTF-8 (stops before invalid UTF-8)
expect (List.concat (Str.toUtf8 "爱") [0xC0, 0x80]) |> parseUtf8 == Ok { codePt: fromU32Unchecked '爱', bytesParsed: 3 }
