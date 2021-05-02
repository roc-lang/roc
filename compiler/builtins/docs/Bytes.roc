interface Bytes
    exposes
        [
            Bytes,
            parseUtf8Usv,
            parseUtf16Usv,
            parseUtf8Grapheme,
            parseUtf16Grapheme,
            parsePastUtf8,
            parsePastUtf16,
            parseU16,
            parseI16,
            parseU32,
            parseI32,
            parseU64,
            parseI64,
            parseU128,
            parseI128
        ]
    imports []


Bytes : [ @Bytes ]

# Conversion

fromList : List U8 -> Bytes

toList : Bytes -> List U8

len : Bytes -> Nat

isEmpty : Bytes -> Bool

## The [endianness](https://en.wikipedia.org/wiki/Endianness) of the currently running system.
hostEndi : Endi

## [Endianness](https://en.wikipedia.org/wiki/Endianness)
##
## Be - Big Endian
## Le - Little Endian
Endi : [ Be, Le ]

# Access

splitFirst : Bytes -> Result { first : U8, rest : Bytes } [ NoBytes ]*

take : Bytes, Nat -> Bytes

# Building

appendLe : Bytes, Num * -> Bytes
appendBe : Bytes, Num * -> Bytes
concat : Bytes, Bytes -> Bytes

# Parsing

## Parse a [Unicode Scalar Value](http://www.unicode.org/glossary/#unicode_scalar_value)
## (USV) encoded as UTF-8.
##
## To parse an entire UTF-8 string, you can use #Bytes.toUtf8 or #Bytes.parsePastUtf8.
parseUsvUtf8 : Bytes -> Result { val : Usv, rest : Bytes } [ Expected [ Utf8Usv ]* Bytes ]*
parseUsvUtf16 : Bytes, Endi -> Result { val : Usv, rest : Bytes } [ Expected [ Utf16Usv Endi ]* Bytes ]*
parseGraphemeUtf8 : Bytes -> Result { val : Str, rest : Bytes } [ Expected [ Utf8Grapheme ]* Bytes ]*
parseGraphemeUtf16 : Bytes, Endi -> Result { val : Str, rest : Bytes } [ Expected [ Utf16Grapheme Endi ]* Bytes ]*

## If the bytes begin with the given UTF-8 string, return whatever bytes come
## after it.
chompUtf8 : Bytes, Str -> Result Bytes [ Expected [ ExactStr Str ]* Bytes ]*
chompUtf16 : Bytes, Endi, Str -> Result Bytes [ Expected [ ExactStr Str ]* Bytes ]*
chompUsvUtf8 : Usv -> Result Str [ Expected [ ExactUsv Usv ]* Bytes ]*
chompUsvUtf16 : Usv, Endi -> Result Str [ Expected [ ExactUsv Usv ]* Bytes ]*
## If the bytes begin with the given bytes, return whatever bytes come
## after them.
chompBytes : Bytes, Bytes -> Result Bytes [ Expected [ ExactStr Str ]* Bytes ]*

parseU16 : Bytes, Endi -> Result { val : U16, rest : Bytes } [ Expected [ U16 Endi ]* Bytes ]*
parseI16 : Bytes, Endi -> Result { val : I16, rest : Bytes } [ Expected [ I16 Endi ]* Bytes ]*
parseU32 : Bytes, Endi -> Result { val : U32, rest : Bytes } [ Expected [ U32 Endi ]* Bytes ]*
parseI32 : Bytes, Endi -> Result { val : I32, rest : Bytes } [ Expected [ I32 Endi ]* Bytes ]*
parseU64 : Bytes, Endi -> Result { val : U64, rest : Bytes } [ Expected [ U64 Endi ]* Bytes ]*
parseI64 : Bytes, Endi -> Result { val : I64, rest : Bytes } [ Expected [ I64 Endi ]* Bytes ]*
parseU128 : Bytes, Endi -> Result { val : U128, rest : Bytes } [ Expected [ U128 Endi ]* Bytes ]*
parseI128 : Bytes, Endi -> Result { val : I128, rest : Bytes } [ Expected [ I128 Endi ]* Bytes ]*
