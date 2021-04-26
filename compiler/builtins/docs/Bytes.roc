interface Bytes
    exposes
        [ Bytes
        ]
    imports []

## Terminology
##
## There are two forms of byte ordering used here:
##
## NBO is short for [Network Byte Ordering](https://en.wikipedia.org/wiki/Endianness#Networking) (also known as ["Big-Endian"](https://en.wikipedia.org/wiki/Endianness) - you can also think of NBO as being short for "Normal Byte Ordering")
## RBO is short for Reverse Byte Ordering (also known as ["Little-Endian"](https://en.wikipedia.org/wiki/Endianness))

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

## The byte ordering of the currently running system.
hostOrdering : [ Nbo, Rbo ]

# Access

splitFirst : Bytes -> Result { first : U8, rest : Bytes } [ NoBytes ]*
take : Bytes, Nat -> Bytes

# Building

appendNbo : Bytes, Num * -> Bytes
appendRbo : Bytes, Num * -> Bytes
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

# Network Byte Order ("Big-Endian")
parseNboU16 : Bytes -> Result { answer : U16, rest : Bytes } [ Expected [ U16 ]* Bytes ]*
parseNboI16 : Bytes -> Result { answer : I16, rest : Bytes } [ Expected [ I16 ]* Bytes ]*
parseNboU32 : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ U32 ]* Bytes ]*
parseNboI32 : Bytes -> Result { answer : I32, rest : Bytes } [ Expected [ I32 ]* Bytes ]*
parseNboU64 : Bytes -> Result { answer : U64, rest : Bytes } [ Expected [ U64 ]* Bytes ]*
parseNboI64 : Bytes -> Result { answer : I64, rest : Bytes } [ Expected [ I64 ]* Bytes ]*
parseNboU128 : Bytes -> Result { answer : U128, rest : Bytes } [ Expected [ U128 ]* Bytes ]*
parseNboI128 : Bytes -> Result { answer : I128, rest : Bytes } [ Expected [ I128 ]* Bytes ]*

# Reverse Byte Order ("Little-Endian")
parseRboU16 : Bytes -> Result { answer : U16, rest : Bytes } [ Expected [ U16 ]* Bytes ]*
parseRboI16 : Bytes -> Result { answer : I16, rest : Bytes } [ Expected [ I16 ]* Bytes ]*
parseRboU32 : Bytes -> Result { answer : U32, rest : Bytes } [ Expected [ U32 ]* Bytes ]*
parseRboI32 : Bytes -> Result { answer : I32, rest : Bytes } [ Expected [ I32 ]* Bytes ]*
parseRboU64 : Bytes -> Result { answer : U64, rest : Bytes } [ Expected [ U64 ]* Bytes ]*
parseRboI64 : Bytes -> Result { answer : I64, rest : Bytes } [ Expected [ I64 ]* Bytes ]*
parseRboU128 : Bytes -> Result { answer : U128, rest : Bytes } [ Expected [ U128 ]* Bytes ]*
parseRboI128 : Bytes -> Result { answer : I128, rest : Bytes } [ Expected [ I128 ]* Bytes ]*
