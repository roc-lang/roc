interface Unicode.CodePoint.Internal
    exposes
        [
            CodePoint,
            toU32,
            fromU32,
            fromU32Unchecked,
        ]
    imports
        []

## This is just here so that both Unicode.Scalar and Unicode.CodePoint can access it.
CodePoint := U32

fromU32Unchecked : U32 -> CodePoint
fromU32Unchecked = \u32 -> @CodePoint u32

toU32 : CodePoint -> U32
toU32 = \@CodePoint u32 -> u32

fromU32 : U32 -> Result CodePoint [ BadCodePoint ]*
