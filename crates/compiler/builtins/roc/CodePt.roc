interface CodePt
    exposes [CodePt, utf8Len, toU32, fromU32, isHighSurrogate, isLowSurrogate, isValidScalar]
    imports [CodePtInternal]

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
        Ok (CodePtInternal.fromU32Unchecked u32)
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
