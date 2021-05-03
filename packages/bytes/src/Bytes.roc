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
            parseI128,
        ]
    imports []

Bytes : List U8

# Parsing

## Parse an exact number of UTF-8 [extended grapheme clusters](http://www.unicode.org/glossary/#extended_grapheme_cluster)
## into a [Str], and return the rest of the bytes after those graphemes.
parseGraphemesUtf8 : Bytes, Nat -> Result { val : Str, rest : Bytes } [ Expected [ Utf8Grapheme ]* Bytes ]*
parseGraphemesUtf16 : Bytes, Endi, Nat -> Result { val : Str, rest : Bytes } [ Expected [ Utf16Grapheme Endi ]* Bytes ]*

## If the bytes begin with the given string encoded as UTF-8, return whatever
## bytes come after.
chompUtf8 : Bytes, Str -> Result Bytes [ Expected [ ExactStr Str ]* Bytes ]*
chompUtf16 : Bytes, Endi, Str -> Result Bytes [ Expected [ ExactStr Str ]* Bytes ]*

## If the bytes begin with the given bytes, return whatever comes after.
chomp : Bytes, Bytes -> Result Bytes [ Expected [ ExactStr Str ]* Bytes ]*

parseU16 : Bytes, Endi -> Result { val : U16, rest : Bytes } [ Expected [ NumU16 Endi ]* Bytes ]*
parseI16 : Bytes, Endi -> Result { val : I16, rest : Bytes } [ Expected [ NumI16 Endi ]* Bytes ]*
parseU32 : Bytes, Endi -> Result { val : U32, rest : Bytes } [ Expected [ NumU32 Endi ]* Bytes ]*
parseI32 : Bytes, Endi -> Result { val : I32, rest : Bytes } [ Expected [ NumI32 Endi ]* Bytes ]*
parseU64 : Bytes, Endi -> Result { val : U64, rest : Bytes } [ Expected [ NumU64 Endi ]* Bytes ]*
parseI64 : Bytes, Endi -> Result { val : I64, rest : Bytes } [ Expected [ NumI64 Endi ]* Bytes ]*
parseU128 : Bytes, Endi -> Result { val : U128, rest : Bytes } [ Expected [ NumU128 Endi ]* Bytes ]*
parseI128 : Bytes, Endi -> Result { val : I128, rest : Bytes } [ Expected [ NumI128 Endi ]* Bytes ]*

parseF64 : Bytes, Endi -> Result { val : U128, rest : Bytes } [ Expected [ NumF64 Endi ]* Bytes ]*
parseF32 : Bytes, Endi -> Result { val : I128, rest : Bytes } [ Expected [ NumF32 Endi ]* Bytes ]*
