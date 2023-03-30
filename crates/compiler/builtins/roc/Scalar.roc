interface Scalar
    exposes [Scalar, toU32, toCodePt, appendToUtf8, parseUtf8, fromCodePt, countUtf8Bytes, fromU32]
    imports [CodePt.{ CodePt }, CodePtInternal]

## A [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value) - that is,
## any [code point](./CodePt#CodePt) except for [high-surrogate](http://www.unicode.org/glossary/#high_surrogate_code_point)
## and [low-surrogate](http://www.unicode.org/glossary/#low_surrogate_code_point) code points.
Scalar := U32
    has [Eq, Hash]

toU32 : Scalar -> U32
toU32 = \@Scalar u32 -> u32

fromU32 : U32 -> Result Scalar [InvalidScalar]
fromU32 = \_u32 ->
    crash "TODO implement" # this can't just delegate to CodePt.fromU32; scalars are a subset of code points

toCodePt : Scalar -> CodePt
toCodePt = \@Scalar u32 ->
    CodePtInternal.fromU32Unchecked u32

## Convert a code point to a scalar value. This can fail if the given
## code point is
fromCodePt : CodePt -> Result Scalar [NonScalarCodePt]
fromCodePt = \codePt ->
    if CodePt.isValidScalar codePt then
        Ok (@Scalar (CodePt.toU32 codePt))
    else
        Err NonScalarCodePt

## Encode a [Scalar] as UTF-8 bytes and append those bytes to an existing list of UTF-8 bytes.
appendToUtf8 : List U8, Scalar -> List U8
appendToUtf8 = \bytes, @Scalar codePt ->
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
countUtf8Bytes : Scalar -> Nat
countUtf8Bytes = \@Scalar codePt ->
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

parseUtf8 : List U8 -> Result { scalar : Scalar, bytesParsed : Nat } [InvalidUtf8]
parseUtf8 = \bytes ->
    result =
        firstByte <- List.first bytes |> Result.try

        if firstByte <= 0b0111_1111 then
            Ok { scalar: @Scalar (Num.toU32 firstByte), bytesParsed: 1 }
        else if firstByte >= 0b1100_0000 && firstByte <= 0b1101_1111 then
            secondByte <- List.get bytes 1 |> Result.try

            if
                Num.bitwiseAnd firstByte 0b11100000 == 0b11000000
                && Num.bitwiseAnd secondByte 0b11000000 == 0b10000000
            then
                answer =
                    firstByte
                    |> Num.bitwiseAnd 0b00011111
                    |> Num.shiftLeftBy 6
                    |> Num.bitwiseOr (Num.bitwiseAnd secondByte 0b00111111)

                if answer >= 0x80 then
                    Ok { scalar: @Scalar (Num.toU32 answer), bytesParsed: 2 }
                else
                    Err InvalidUtf8
            else
                Err InvalidUtf8
        else if firstByte >= 0b1110_0000 && firstByte <= 0b1110_1111 then
            secondByte <- List.get bytes 1 |> Result.try
            thirdByte <- List.get bytes 2 |> Result.try

            if
                Num.bitwiseAnd firstByte 0b11110000 == 0b11100000
                && Num.bitwiseAnd secondByte 0b11000000 == 0b10000000
                && Num.bitwiseAnd thirdByte 0b11000000 == 0b10000000
            then
                answer =
                    firstByte
                    |> Num.bitwiseAnd 0b00001111
                    |> Num.shiftLeftBy 12
                    |> Num.bitwiseOr (Num.bitwiseAnd secondByte 0b00111111 |> Num.shiftLeftBy 6)
                    |> Num.bitwiseOr (Num.bitwiseAnd thirdByte 0b00111111)

                if Num.toU16 answer >= 0x800 then
                    Ok { scalar: @Scalar (Num.toU32 answer), bytesParsed: 3 }
                else
                    Err InvalidUtf8
            else
                Err InvalidUtf8
        else if firstByte >= 0b1111_0000 && firstByte <= 0b1111_0111 then
            secondByte <- List.get bytes 1 |> Result.try
            thirdByte <- List.get bytes 2 |> Result.try
            fourthByte <- List.get bytes 3 |> Result.try

            if
                Num.bitwiseAnd firstByte 0b11111000 == 0b11110000
                && Num.bitwiseAnd secondByte 0b11000000 == 0b10000000
                && Num.bitwiseAnd thirdByte 0b11000000 == 0b10000000
                && Num.bitwiseAnd fourthByte 0b11000000 == 0b10000000
            then
                answer =
                    firstByte
                    |> Num.bitwiseAnd 0b00000111
                    |> Num.shiftLeftBy 18
                    |> Num.bitwiseOr (Num.bitwiseAnd secondByte 0b00111111 |> Num.shiftLeftBy 12)
                    |> Num.bitwiseOr (Num.bitwiseAnd thirdByte 0b00111111 |> Num.shiftLeftBy 6)
                    |> Num.bitwiseOr (Num.bitwiseAnd fourthByte 0b00111111)

                if Num.toU32 answer >= 0x10000 && Num.toU32 answer <= 0x10FFFF then
                    Ok { scalar: @Scalar (Num.toU32 answer), bytesParsed: 4 }
                else
                    Err InvalidUtf8
            else
                Err InvalidUtf8
        else
            Err InvalidUtf8

    Result.mapErr result \_ -> InvalidUtf8

## Empty input
expect [] |> parseUtf8 == Err InvalidUtf8

## Incorrect continuation byte
expect [0xC3, 0x28] |> parseUtf8 == Err InvalidUtf8

## Overlong encoding for ASCII character
expect [0xC0, 0xA1] |> parseUtf8 == Err InvalidUtf8

## Overlong encoding for two-byte character
expect [0xE0, 0x80, 0xAF] |> parseUtf8 == Err InvalidUtf8

## Overlong encoding for three-byte character
expect [0xF0, 0x80, 0x80, 0xA6] |> parseUtf8 == Err InvalidUtf8

## Invalid four-byte encoding (scalar value too large)
expect [0xF4, 0x90, 0x80, 0x80] |> parseUtf8 == Err InvalidUtf8

## Invalid first byte (>= 0xF8)
expect [0xF8, 0xA1, 0xA2, 0xA3, 0xA4] |> parseUtf8 == Err InvalidUtf8

## Invalid first byte (>= 0xC0 and <= 0xC1)
expect [0xC0, 0x80] |> parseUtf8 == Err InvalidUtf8
