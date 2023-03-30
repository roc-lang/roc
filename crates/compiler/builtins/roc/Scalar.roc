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
