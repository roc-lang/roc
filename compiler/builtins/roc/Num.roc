interface Num
    exposes
        [
            Num,
            Int,
            Float,

            Integer,
            FloatingPoint,

            I128,
            I64,
            I32,
            I16,
            I8,

            U128,
            U64,
            U32,
            U16,
            U8,

            Signed128,
            Signed64,
            Signed32,
            Signed16,
            Signed8,

            Unsigned128,
            Unsigned64,
            Unsigned32,
            Unsigned16,
            Unsigned8,

            Nat,
            Dec,

            F32,
            F64,

            Natural,
            Decimal,

            Binary32,
            Binary64,

            abs,
            neg,
            add,
            sub,
            mul,
            isLt,
            isLte,
            isGt,
            isGte,
            sin,
            cos,
            tan,
            atan,
            acos,
            asin,
            isZero,
            isEven,
            isOdd,
            toFloat,
            isPositive,
            isNegative,
            rem,
            div,
            divChecked,
            sqrt,
            log,
            round,
            ceiling,
            floor,
            compare,
            pow,
            powInt,
            addWrap,
            addChecked,
            addSaturated,
            bitwiseAnd,
            bitwiseXor,
            bitwiseOr,
            shiftLeftBy,
            shiftRightBy,
            shiftRightZfBy,
            subWrap,
            subChecked,
            subSaturated,
            mulWrap,
            mulChecked,
            intCast,
            bytesToU16,
            bytesToU32,
            divCeil,
            divCeilChecked,
            divFloor,
            divFloorChecked,
            toStr,
            isMultipleOf,
            minI8,
            maxI8,
            minU8,
            maxU8,
            minI16,
            maxI16,
            minU16,
            maxU16,
            minI32,
            maxI32,
            minU32,
            maxU32,
            minI64,
            maxI64,
            minU64,
            maxU64,
            minI128,
            maxI128,
            minU128,
            maxU128,
            minF32,
            maxF32,
            minF64,
            maxF64,
            toI8,
            toI8Checked,
            toI16,
            toI16Checked,
            toI32,
            toI32Checked,
            toI64,
            toI64Checked,
            toI128,
            toI128Checked,
            toU8,
            toU8Checked,
            toU16,
            toU16Checked,
            toU32,
            toU32Checked,
            toU64,
            toU64Checked,
            toU128,
            toU128Checked,
            toNat,
            toNatChecked,
            toF32,
            toF32Checked,
            toF64,
            toF64Checked,
        ]
    imports
        [
            Bool.{ Bool }
        ]

Num range : [ @Num range ]
Int range : Num (Integer range)
Float range : Num (FloatingPoint range)

Signed128 : [ @Signed128 ]
Signed64 : [ @Signed64 ]
Signed32 : [ @Signed32 ]
Signed16 : [ @Signed16 ]
Signed8 : [ @Signed8 ]

Unsigned128 : [ @Unsigned128 ]
Unsigned64 : [ @Unsigned64 ]
Unsigned32 : [ @Unsigned32 ]
Unsigned16 : [ @Unsigned16 ]
Unsigned8 : [ @Unsigned8 ]

Natural : [ @Natural ]

Integer range : [ @Integer range ]

I128 : Num (Integer Signed128)
I64 : Num (Integer Signed64)
I32 : Num (Integer Signed32)
I16 : Num (Integer Signed16)
I8 : Int Signed8

U128 : Num (Integer Unsigned128)
U64 : Num (Integer Unsigned64)
U32 : Num (Integer Unsigned32)
U16 : Num (Integer Unsigned16)
U8 : Num (Integer Unsigned8)

Nat : Num (Integer Natural)

Decimal : [ @Decimal ]
Binary64 : [ @Binary64 ]
Binary32 : [ @Binary32 ]

FloatingPoint range : [ @FloatingPoint range ]

F64 : Num (FloatingPoint Binary64)
F32 : Num (FloatingPoint Binary32)
Dec : Num (FloatingPoint Decimal)

# ------- Functions

toStr : Num * -> Str
intCast : Int a -> Int b

bytesToU16 : List U8, Nat -> Result U16 [ OutOfBounds ]
bytesToU32 : List U8, Nat -> Result U32 [ OutOfBounds ]

compare : Num a, Num a -> [ LT, EQ, GT ]

isLt : Num a, Num a -> Bool
isGt : Num a, Num a -> Bool
isLte : Num a, Num a -> Bool
isGte : Num a, Num a -> Bool

isZero : Num a -> Bool

isEven : Int a -> Bool
isOdd : Int a -> Bool

isPositive : Num a -> Bool
isNegative : Num a -> Bool

toFloat : Num * -> Float *

abs : Num a -> Num a
neg : Num a -> Num a

add : Num a, Num a -> Num a
sub : Num a, Num a -> Num a
mul : Num a, Num a -> Num a

sin : Float a -> Float a
cos : Float a -> Float a
tan : Float a -> Float a

asin : Float a -> Float a
acos : Float a -> Float a
atan : Float a -> Float a

sqrt : Float a -> Result (Float a) [ SqrtOfNegative ]*
log : Float a -> Result (Float a) [ LogNeedsPositive ]*
div : Float a, Float a -> Float a
divChecked : Float a, Float a -> Result (Float a) [ DivByZero ]*

divCeil : Int a, Int a -> Int a
divCeilChecked : Int a, Int a -> Result (Int a) [ DivByZero ]*
divFloor : Int a, Int a -> Int a
divFloorChecked : Int a, Int a -> Result (Int a) [ DivByZero ]*
# mod : Float a, Float a -> Result (Float a) [ DivByZero ]*

rem : Int a, Int a -> Result (Int a) [ DivByZero ]*
# mod : Int a, Int a -> Result (Int a) [ DivByZero ]*
isMultipleOf : Int a, Int a -> Bool

bitwiseAnd : Int a, Int a -> Int a
bitwiseXor : Int a, Int a -> Int a
bitwiseOr : Int a, Int a -> Int a
shiftLeftBy : Int a, Int a -> Int a
shiftRightBy : Int a, Int a -> Int a
shiftRightZfBy : Int a, Int a -> Int a

round : Float * -> Int *
floor : Float * -> Int *
ceiling : Float * -> Int *

pow : Float a, Float a -> Float a
powInt : Int a, Int a -> Int a

addWrap : Int range, Int range -> Int range
addSaturated : Num a, Num a -> Num a
addChecked : Num a, Num a -> Result (Num a) [ Overflow ]*

subWrap : Int range, Int range -> Int range
subSaturated : Num a, Num a -> Num a
subChecked : Num a, Num a -> Result (Num a) [ Overflow ]*

mulWrap : Int range, Int range -> Int range
# mulSaturated : Num a, Num a -> Num a
mulChecked : Num a, Num a -> Result (Num a) [ Overflow ]*

minI8 : I8
minI8 = -128i8

maxI8 : I8
maxI8 = 127i8

minU8 : U8
minU8 = 0u8

maxU8 : U8
maxU8 = 255u8

minI16 : I16
minI16 = -32768i16

maxI16 : I16
maxI16 = 32767i16

minU16 : U16
minU16 = 0u16

maxU16 : U16
maxU16 = 65535u16

minI32 : I32
minI32 = -2147483648

maxI32 : I32
maxI32 = 2147483647

minU32 : U32
minU32 = 0

maxU32 : U32
maxU32 = 4294967295

minI64 : I64
minI64 = -9223372036854775808

maxI64 : I64
maxI64 = 9223372036854775807

minU64 : U64
minU64 = 0

maxU64 : U64
maxU64 = 18446744073709551615

minI128 : I128
minI128 = -170141183460469231731687303715884105728

maxI128 : I128
maxI128 = 170141183460469231731687303715884105727

minU128 : U128
minU128 = 0

maxU128 : U128
maxU128 = 0340282366920938463463374607431768211455

minF32 : F32
minF32 = -3.40282347e38

maxF32 : F32
maxF32 = 3.40282347e38

minF64 : F64
minF64 = -1.7976931348623157e308

maxF64 : F64
maxF64 = 1.7976931348623157e308

toI8 : Int * -> I8
toI16 : Int * -> I16
toI32 : Int * -> I32
toI64 : Int * -> I64
toI128 : Int * -> I128
toU8 : Int * -> U8
toU16 : Int * -> U16
toU32 : Int * -> U32
toU64 : Int * -> U64
toU128 : Int * -> U128
toNat : Int * -> Nat

toF32 : Num * -> F32
toF64 : Num * -> F64

toI8Checked : Int * -> Result I8 [ OutOfBounds ]*
toI16Checked : Int * -> Result I16 [ OutOfBounds ]*
toI32Checked : Int * -> Result I32 [ OutOfBounds ]*
toI64Checked : Int * -> Result I64 [ OutOfBounds ]*
toI128Checked : Int * -> Result I128 [ OutOfBounds ]*
toU8Checked : Int * -> Result U8 [ OutOfBounds ]*
toU16Checked : Int * -> Result U16 [ OutOfBounds ]*
toU32Checked : Int * -> Result U32 [ OutOfBounds ]*
toU64Checked : Int * -> Result U64 [ OutOfBounds ]*
toU128Checked : Int * -> Result U128 [ OutOfBounds ]*
toNatChecked : Int * -> Result Nat [ OutOfBounds ]*
toF32Checked : Num * -> Result F32 [ OutOfBounds ]*
toF64Checked : Num * -> Result F64 [ OutOfBounds ]*
