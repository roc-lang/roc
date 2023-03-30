interface CodePtInternal
    exposes [CodePtInternal, fromU32Unchecked, toU32]
    imports []

CodePtInternal := U32
    has [Eq, Hash]

toU32 : CodePtInternal -> U32
toU32 = \@CodePtInternal u32 -> u32

fromU32Unchecked : U32 -> CodePtInternal
fromU32Unchecked = @CodePtInternal
