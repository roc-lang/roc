interface Str
    exposes 
        [ 
            concat,
            Utf8Problem,
            Utf8ByteProblem,
            isEmpty,
            joinWith,
            split,
            repeat,
            countGraphemes,
            startsWithCodePt,
            toUtf8,
            fromUtf8,
            fromUtf8Range,
            startsWith,
            endsWith,
            trim,
            trimLeft,
            trimRight,

            toDec,
            toF64,
            toF32,
            toNat,
            toU128,
            toI128,
            toU64,
            toI64,
            toU32,
            toI32,
            toU16,
            toI16,
            toU8,
            toI8,
        ]
    imports [ Bool.{ Bool }, Result.{ Result } ]



Utf8ByteProblem :
    [
        InvalidStartByte,
        UnexpectedEndOfSequence,
        ExpectedContinuation,
        OverlongEncoding,
        CodepointTooLarge,
        EncodesSurrogateHalf,
    ]

Utf8Problem : { byteIndex : Nat, problem : Utf8ByteProblem }

isEmpty : Str -> Bool
concat : Str, Str -> Str

joinWith : List Str, Str -> Str
split : Str, Str -> List Str
repeat : Str, Nat -> Str
countGraphemes : Str -> Nat
startsWithCodePt : Str, U32 -> Bool

toUtf8 : Str -> List U8

# fromUtf8 : List U8 -> Result Str [ BadUtf8 Utf8Problem ]*
# fromUtf8Range : List U8 -> Result Str [ BadUtf8 Utf8Problem Nat, OutOfBounds ]*

fromUtf8 : List U8 -> Result Str [ BadUtf8 Utf8ByteProblem Nat ]*
fromUtf8Range : List U8, { start : Nat, count : Nat } -> Result Str [ BadUtf8 Utf8ByteProblem Nat, OutOfBounds ]*

startsWith : Str, Str -> Bool
endsWith : Str, Str -> Bool

trim : Str -> Str
trimLeft : Str -> Str
trimRight : Str -> Str

toDec : Str -> Result Dec [ InvalidNumStr ]*
toF64 : Str -> Result F64 [ InvalidNumStr ]*
toF32 : Str -> Result F32 [ InvalidNumStr ]*
toNat : Str -> Result Nat [ InvalidNumStr ]*
toU128 : Str -> Result U128 [ InvalidNumStr ]*
toI128 : Str -> Result I128 [ InvalidNumStr ]*
toU64 : Str -> Result U64 [ InvalidNumStr ]*
toI64 : Str -> Result I64 [ InvalidNumStr ]*
toU32 : Str -> Result U32 [ InvalidNumStr ]*
toI32 : Str -> Result I32 [ InvalidNumStr ]*
toU16 : Str -> Result U16 [ InvalidNumStr ]*
toI16 : Str -> Result I16 [ InvalidNumStr ]*
toU8 : Str -> Result U8 [ InvalidNumStr ]*
toI8 : Str -> Result I8 [ InvalidNumStr ]*
