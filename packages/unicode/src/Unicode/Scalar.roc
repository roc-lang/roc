interface Unicode.Scalar
    exposes
        [
            Scalar,
            toStr,
            toCodePt,
            fromCodePt,
            parseUtf8,
            parseUtf16,
            chompUtf8,
            chompUtf16
        ]
    imports
        [
            Unicode.CodePt.Internal as Internal
            Unicode.CodePt.{ CodePt },
            Bytes.{ Bytes }
        ]

## A [Unicode Scalar Value](http://www.unicode.org/glossary/#unicode_scalar_value)
Scalar := U32

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

toCodePt : Scalar -> CodePt
toCodePt = \@Scalar u32 -> Internal.fromU32Unchecked u32

fromCodePt : CodePt -> Result Scalar [ PointWasSurrogate ]*

parseUtf8 : Bytes -> Result { val : Scalar, rest : Bytes } [ Expected [ Utf8CodePt ]* Bytes ]*
parseUtf16 : Bytes, Endi -> Result { val : Scalar, rest : Bytes } [ Expected [ Utf16CodePt Endi ]* Bytes ]*

chompUtf8 : Bytes, CodePt -> Result Str [ Expected [ ExactCodePt CodePt ]* Bytes ]*
chompUtf16 : Bytes, CodePt, Endi -> Result Str [ Expected [ ExactCodePt CodePt ]* Bytes ]*

isAsciiDigit : CodePt -> Bool
