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
            parseLeU16,
            parseLeI16,
            parseLeU32,
            parseLeI32,
            parseLeU64,
            parseLeI64,
            parseLeU128,
            parseLeI128,
            parseBeU16,
            parseBeI16,
            parseBeU32,
            parseBeI32,
            parseBeU64,
            parseBeI64,
            parseBeU128,
            parseBeI128
        ]
    imports []

# Conversion

fromList : List U8 -> Bytes

toList : Bytes -> List U8

len : Bytes -> Nat

isEmpty : Bytes -> Bool

## The endianness of the currently running system.
hostEndianness : [ Big, Little ]

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
parseUsvUtf8 : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ Utf8Usv ]* Bytes ]*
parseUsvUtf16Le : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ Utf16BeUsv ]* Bytes ]*
parseUsvUtf16Be : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ Utf16BeUsv ]* Bytes ]*
parseGraphemeUtf8 : Bytes -> Result { answer : Str, rest : Bytes } [ Expected [ Utf8Grapheme ]* Bytes ]*
parseGraphemeUtf16Le : Bytes -> Result { answer : Str, rest : Bytes } [ Expected [ Utf16LeGrapheme ]* Bytes ]*
parseGraphemeUtf16Be : Bytes -> Result { answer : Str, rest : Bytes } [ Expected [ Utf16BeGrapheme ]* Bytes ]*

## If the bytes begin with the given string, return whatever bytes come
## after it.
parsePastStr : Bytes, Str -> Result Bytes [ Expected [ ExactStr Str ]* Bytes ]*

# Little-Endian
parseU16Le : Bytes -> Result { answer : U16, rest : Bytes } [ Expected [ U16 ]* Bytes ]*
parseI16Le : Bytes -> Result { answer : I16, rest : Bytes } [ Expected [ I16 ]* Bytes ]*
parseU32Le : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ U32 ]* Bytes ]*
parseI32Le : Bytes -> Result { answer : I32, rest : Bytes } [ Expected [ I32 ]* Bytes ]*
parseU64Le : Bytes -> Result { answer : U64, rest : Bytes } [ Expected [ U64 ]* Bytes ]*
parseI64Le : Bytes -> Result { answer : I64, rest : Bytes } [ Expected [ I64 ]* Bytes ]*
parseU128Le : Bytes -> Result { answer : U128, rest : Bytes } [ Expected [ U128 ]* Bytes ]*
parseI128Le : Bytes -> Result { answer : I128, rest : Bytes } [ Expected [ I128 ]* Bytes ]*

# Big-Endian
parseU16Be : Bytes -> Result { answer : U16, rest : Bytes } [ Expected [ U16 ]* Bytes ]*
parseI16Be : Bytes -> Result { answer : I16, rest : Bytes } [ Expected [ I16 ]* Bytes ]*
parseU32Be : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ U32 ]* Bytes ]*
parseI32Be : Bytes -> Result { answer : I32, rest : Bytes } [ Expected [ I32 ]* Bytes ]*
parseU64Be : Bytes -> Result { answer : U64, rest : Bytes } [ Expected [ U64 ]* Bytes ]*
parseI64Be : Bytes -> Result { answer : I64, rest : Bytes } [ Expected [ I64 ]* Bytes ]*
parseU128Be : Bytes -> Result { answer : U128, rest : Bytes } [ Expected [ U128 ]* Bytes ]*
parseI128Be : Bytes -> Result { answer : I128, rest : Bytes } [ Expected [ I128 ]* Bytes ]*
