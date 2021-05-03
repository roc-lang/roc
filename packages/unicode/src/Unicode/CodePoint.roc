interface Unicode.CodePoint
    exposes
        [
            CodePoint,
            toU32,
            fromU32,
            parseUtf8,
            parseUtf16,
            chompUtf8,
            chompUtf16
        ]
    imports
        [
            Unicode.CodePoint.Internal as Internal
        ]

## A [Unicode Code Point](http://www.unicode.org/glossary/#code_point)
CodePoint : Internal.CodePoint

toU32 : CodePoint -> U32
toU32 = \codePoint -> Internal.toU32 codePoint

## To convert exactly one [CodePoint] to a [Str], that code point must be
## a valid [Unicode Scalar Value](http://www.unicode.org/glossary/#unicode_scalar_value).
## You can get one of those by calling [Unicode.Scalar.fromCodePoint], and then
## you can call [Unicode.Scalar.toStr] to get a [Str] from it.
toStr : List CodePoint -> Result Str [ BadCodePoint U32 ]*
toStr = \points ->
    u32s = List.map points toU32

    Str.fromCodePoints u32s

fromU32 : U32 -> Result CodePoint [ BadCodePoint ]*

parseUtf8 : Bytes -> Result { val : CodePoint, rest : Bytes } [ Expected [ Utf8CodePoint ]* Bytes ]*
parseUtf16 : Bytes, Endi -> Result { val : CodePoint, rest : Bytes } [ Expected [ Utf16CodePoint Endi ]* Bytes ]*

chompUtf8 : Bytes, CodePoint -> Result Str [ Expected [ ExactCodePoint CodePoint ]* Bytes ]*
chompUtf16 : Bytes, CodePoint, Endi -> Result Str [ Expected [ ExactCodePoint CodePoint ]* Bytes ]*

isAsciiDigit : CodePoint -> Bool
