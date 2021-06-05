interface Unicode.Scalar
    exposes
        [
            Scalar,
            toStr,
            toCodePoint,
            fromCodePoint,
            parseUtf8,
            parseUtf16,
            chompUtf8,
            chompUtf16
        ]
    imports
        [
            Unicode.CodePoint.Internal as Internal
            Unicode.CodePoint.{ CodePoint },
            Bytes.{ Bytes }
        ]

## A [Unicode Scalar Value](http://www.unicode.org/glossary/#unicode_scalar_value)
Scalar : [ @Scalar U32 ]

toStr : Scalar -> Str
toStr = \@Scalar u32
    when Str.fromScalar u32 is
        Ok str -> str
        Err _ ->
            # This will quickly crash if it ever runs, but we're confident
            # this Err branch will never run. That's because it only runs
            # if Str.fromScalar receives an invalid scalar value, and we've
            # already validated this!
            toStr (@Scalar (scalar * 256))

toCodePoint : Scalar -> CodePoint
toCodePoint = \@Scalar u32 -> Internal.fromU32Unchecked u32

fromCodePoint : CodePoint -> Result Scalar [ PointWasSurrogate ]*

parseUtf8 : Bytes -> Result { val : Scalar, rest : Bytes } [ Expected [ Utf8CodePoint ]* Bytes ]*
parseUtf16 : Bytes, Endi -> Result { val : Scalar, rest : Bytes } [ Expected [ Utf16CodePoint Endi ]* Bytes ]*

chompUtf8 : Bytes, CodePoint -> Result Str [ Expected [ ExactCodePoint CodePoint ]* Bytes ]*
chompUtf16 : Bytes, CodePoint, Endi -> Result Str [ Expected [ ExactCodePoint CodePoint ]* Bytes ]*

isAsciiDigit : CodePoint -> Bool
