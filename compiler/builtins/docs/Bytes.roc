interface Bytes
    exposes
        [ Bytes
        ]
    imports []

# Conversion

## Return a #List of the string's #U8 UTF-8 [code units](https://unicode.org/glossary/#code_unit).
## (To split the string into a #List of smaller #Str values instead of #U8 values,
## see #Str.split and #Str.graphemes.)
##
## >>> Str.toUtf8 "👩‍👩‍👦‍👦"
##
## >>> Str.toUtf8 "Roc"
##
## >>> Str.toUtf8 "鹏"
##
## >>> Str.toUtf8 "🐦"
##
## For a more flexible function that walks through each of these #U8 code units
## without creating a #List, see #Str.walkUtf8 and #Str.walkRevUtf8.
fromStr : Str * -> Bytes

fromList : List U8 -> Bytes

toList : Bytes -> List U8

toUtf8 : Bytes -> Result Utf8 [ BadUtf8 ]*

toUtf16 : Bytes -> Result Utf16 [ BadUtf16 ]*

## The number of bytes a string uses in memory.
##
## >>> Bytes.inStr "👩‍👩‍👦‍👦"
##
## >>> Bytes.inStr "Roc"
##
## >>> Bytes.inStr "鹏"
##
## >>> Bytes.inStr "🐦"
inStr : Str * -> Nat

len : Bytes -> Nat

isEmpty : Bytes -> Bool

## The endianness of the currently running system.
hostEndianness : [ Big, Little ]

# Access

splitFirst : Bytes -> Result { first : U8, rest : Bytes } [ NoBytes ]*
take : Bytes, Nat -> Bytes

# Building

## Append a number to the bytes, encoded using host endianness.
appendHe : Bytes, Num * -> Bytes
appendLe : Bytes, Num * -> Bytes
appendBe : Bytes, Num * -> Bytes
concat : Bytes, Bytes -> Bytes

# Parsing


## Parse a [Unicode Scalar Value](http://www.unicode.org/glossary/#unicode_scalar_value)
## (USV) encoded as UTF-8.
##
## To parse a fixed-length UTF-8 string, you can use #Bytes.take and #Bytes.toUtf8.
parseUtf8Usv : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ Utf8Usv ]* Bytes ]*
parseUtf16Usv : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ Utf16Usv ]* Bytes ]*
parseUtf8Grapheme : Bytes -> Result { answer : Utf8, rest : Bytes } [ Expected [ Utf8Grapheme ]* Bytes ]*
parseUtf16Grapheme : Bytes -> Result { answer : Utf16, rest : Bytes } [ Expected [ Utf16Grapheme ]* Bytes ]*

# Little-Endian
parseLeU16 : Bytes -> Result { answer : U16, rest : Bytes } [ Expected [ U16 ]* Bytes ]*
parseLeI16 : Bytes -> Result { answer : I16, rest : Bytes } [ Expected [ I16 ]* Bytes ]*
parseLeU32 : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ U32 ]* Bytes ]*
parseLeI32 : Bytes -> Result { answer : I32, rest : Bytes } [ Expected [ I32 ]* Bytes ]*
parseLeU64 : Bytes -> Result { answer : U64, rest : Bytes } [ Expected [ U64 ]* Bytes ]*
parseLeI64 : Bytes -> Result { answer : I64, rest : Bytes } [ Expected [ I64 ]* Bytes ]*
parseLeU128 : Bytes -> Result { answer : U128, rest : Bytes } [ Expected [ U128 ]* Bytes ]*
parseLeI128 : Bytes -> Result { answer : I128, rest : Bytes } [ Expected [ I128 ]* Bytes ]*

# Big-Endian
parseBeU16 : Bytes -> Result { answer : U16, rest : Bytes } [ Expected [ U16 ]* Bytes ]*
parseBeI16 : Bytes -> Result { answer : I16, rest : Bytes } [ Expected [ I16 ]* Bytes ]*
parseBeU32 : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ U32 ]* Bytes ]*
parseBeI32 : Bytes -> Result { answer : I32, rest : Bytes } [ Expected [ I32 ]* Bytes ]*
parseBeU64 : Bytes -> Result { answer : U64, rest : Bytes } [ Expected [ U64 ]* Bytes ]*
parseBeI64 : Bytes -> Result { answer : I64, rest : Bytes } [ Expected [ I64 ]* Bytes ]*
parseBeU128 : Bytes -> Result { answer : U128, rest : Bytes } [ Expected [ U128 ]* Bytes ]*
parseBeI128 : Bytes -> Result { answer : I128, rest : Bytes } [ Expected [ I128 ]* Bytes ]*

# Host Endiannesss
parseHeU16 : Bytes -> Result { answer : U16, rest : Bytes } [ Expected [ U16 ]* Bytes ]*
parseHeI16 : Bytes -> Result { answer : I16, rest : Bytes } [ Expected [ I16 ]* Bytes ]*
parseHeU32 : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ U32 ]* Bytes ]*
parseHeI32 : Bytes -> Result { answer : I32, rest : Bytes } [ Expected [ I32 ]* Bytes ]*
parseHeU64 : Bytes -> Result { answer : U64, rest : Bytes } [ Expected [ U64 ]* Bytes ]*
parseHeI64 : Bytes -> Result { answer : I64, rest : Bytes } [ Expected [ I64 ]* Bytes ]*
parseHeU128 : Bytes -> Result { answer : U128, rest : Bytes } [ Expected [ U128 ]* Bytes ]*
parseHeI128 : Bytes -> Result { answer : I128, rest : Bytes } [ Expected [ I128 ]* Bytes ]*
