interface Num
    exposes [
        Num,
        Int,
        Frac,
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
        toFrac,
        isPositive,
        isNegative,
        rem,
        remChecked,
        div,
        divChecked,
        sqrt,
        sqrtChecked,
        log,
        logChecked,
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
        mulSaturated,
        mulChecked,
        intCast,
        bytesToU16,
        bytesToU32,
        divCeil,
        divCeilChecked,
        divTrunc,
        divTruncChecked,
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
    imports [
        Bool.{ Bool },
    ]

## Represents a number that could be either an [Int] or a [Frac].
##
## This is useful for functions that can work on either, for example #Num.add, whose type is:
##
## ```
## add : Num a, Num a -> Num a
## ```
##
## The number 1.5 technically has the type `Num (Fraction *)`, so when you pass
## two of them to [Num.add], the answer you get is `3.0 : Num (Fraction *)`.
##
## Similarly, the number 0x1 (that is, the integer 1 in hexadecimal notation)
## technically has the type `Num (Integer *)`, so when you pass two of them to
## [Num.add], the answer you get is `2 : Num (Integer *)`.
##
## The type [`Frac a`](#Frac) is defined to be an alias for `Num (Fraction a)`,
## so `3.0 : Num (Fraction *)` is the same value as `3.0 : Frac *`.
## Similarly, the type [`Int a`](#Int) is defined to be an alias for
## `Num (Integer a)`, so `2 : Num (Integer *)` is the same value as
## `2 : Int *`.
##
## In this way, the [Num] type makes it possible to have `1 + 0x1` return
## `2 : Int *` and `1.5 + 1.5` return `3.0 : Frac`.
##
## ## Number Literals
##
## Number literals without decimal points (like `0`, `4` or `360`)
## have the type `Num *` at first, but usually end up taking on
## a more specific type based on how they're used.
##
## For example, in `(1 + List.len myList)`, the `1` has the type `Num *` at first,
## but because `List.len` returns a `Nat`, the `1` ends up changing from
## `Num *` to the more specific `Nat`, and the expression as a whole
## ends up having the type `Nat`.
##
## Sometimes number literals don't become more specific. For example,
## the `Num.toStr` function has the type `Num * -> Str`. This means that
## when calling `Num.toStr (5 + 6)`, the expression `(5 + 6)`
## still has the type `Num *`. When this happens, `Num *` defaults to
## being an [I64] - so this addition expression would overflow
## if either 5 or 6 were replaced with a number big enough to cause
## addition overflow on an [I64] value.
##
## If this default of [I64] is not big enough for your purposes,
## you can add an `i128` to the end of the number literal, like so:
##
## >>> Num.toStr 5_000_000_000i128
##
## This `i128` suffix specifies that you want this number literal to be
## an [I128] instead of a `Num *`. All the other numeric types have
## suffixes just like `i128`; here are some other examples:
##
## * `215u8` is a `215` value of type [U8]
## * `76.4f32` is a `76.4` value of type [F32]
## * `123.45dec` is a `123.45` value of type [Dec]
## * `12345nat` is a `12345` value of type [Nat]
##
## In practice, these are rarely needed. It's most common to write
## number literals without any suffix.
Num range := range

## A fixed-size integer - that is, a number with no fractional component.
##
## Integers come in two flavors: signed and unsigned. Signed integers can be
## negative ("signed" refers to how they can incorporate a minus sign),
## whereas unsigned integers cannot be negative.
##
## Since integers have a fixed size, the size you choose determines both the
## range of numbers it can represent, and also how much memory it takes up.
##
## [U8] is an an example of an integer. It is an unsigned [Int] that takes up 8 bits
## (aka 1 byte) in memory. The `U` is for Unsigned and the 8 is for 8 bits.
## Because it has 8 bits to work with, it can store 256 numbers (2^8),
## and because it is unsigned, its min value is 0. This means the 256 numbers
## it can store range from 0 to 255.
##
## [I8] is a signed integer that takes up 8 bits. The `I` is for Integer, since
## integers in mathematics are signed by default. Because it has 8 bits just
## like [U8], it can store 256 numbers (still 2^16), but because it is signed,
## the range is different. Its 256 numbers range from -128 to 127.
##
## Here are some other examples:
##
## * [U16] is like [U8], except it takes up 16 bits in memory. It can store 65,536 numbers (2^16), ranging from 0 to 65,536.
## * [I16] is like [U16], except it is signed. It can still store the same 65,536 numbers (2^16), ranging from -32,768 to 32,767.
##
## This pattern continues up to [U128] and [I128].
##
## ## Performance notes
##
## In general, using smaller numeric sizes means your program will use less memory.
## However, if a mathematical operation results in an answer that is too big
## or too small to fit in the size available for that answer (which is typically
## the same size as the inputs), then you'll get an overflow error.
##
## As such, minimizing memory usage without causing overflows involves choosing
## number sizes based on your knowledge of what numbers you expect your program
## to encounter at runtime.
##
## Minimizing memory usage does not imply maximum runtime speed!
## CPUs are typically fastest at performing integer operations on integers that
## are the same size as that CPU's native machine word size. That means a 64-bit
## CPU is typically fastest at executing instructions on [U64] and [I64] values,
## whereas a 32-bit CPU is typically fastest on [U32] and [I32] values.
##
## Putting these factors together, here are some reasonable guidelines for optimizing performance through integer size choice:
##
## * Start by deciding if this integer should allow negative numbers, and choose signed or unsigned accordingly.
## * Next, think about the range of numbers you expect this number to hold. Choose the smallest size you will never expect to overflow, no matter the inputs your program receives. (Validating inputs for size, and presenting the user with an error if they are too big, can help guard against overflow.)
## * Finally, if a particular numeric calculation is running too slowly, you can try experimenting with other number sizes. This rarely makes a meaningful difference, but some processors can operate on different number sizes at different speeds.
##
## All number literals without decimal points are compatible with [Int] values.
##
## >>> 1
##
## >>> 0
##
## You can optionally put underscores in your [Int] literals.
## They have no effect on the number's value, but can make large numbers easier to read.
##
## >>> 1_000_000
##
## Integers come in two flavors: *signed* and *unsigned*.
##
## * *Unsigned* integers can never be negative. The lowest value they can hold is zero.
## * *Signed* integers can be negative.
##
## Integers also come in different sizes. Choosing a size depends on your performance
## needs and the range of numbers you need to represent. At a high level, the
## general trade-offs are:
##
## * Larger integer sizes can represent a wider range of numbers. If you absolutely need to represent numbers in a certain range, make sure to pick an integer size that can hold them!
## * Smaller integer sizes take up less memory. This savings rarely matters in variables and function arguments, but the sizes of integers that you use in data structures can add up. This can also affect whether those data structures fit in [cache lines](https://en.wikipedia.org/wiki/CPU_cache#Cache_performance), which can be a performance bottleneck.
## * Certain CPUs work faster on some numeric sizes than others. If the CPU is taking too long to run numeric calculations, you may find a performance improvement by experimenting with numeric sizes that are larger than otherwise necessary. However, in practice, doing this typically degrades overall performance, so be careful to measure properly!
##
## Here are the different fixed size integer types:
##
## | Range                                                  | Type  | Size     |
## |--------------------------------------------------------|-------|----------|
## | `                                                -128` | [I8]  | 1 Byte   |
## | `                                                 127` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                                   0` | [U8]  | 1 Byte   |
## | `                                                 255` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                             -32_768` | [I16] | 2 Bytes  |
## | `                                              32_767` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                                   0` | [U16] | 2 Bytes  |
## | `                                              65_535` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                      -2_147_483_648` | [I32] | 4 Bytes  |
## | `                                       2_147_483_647` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                                   0` | [U32] | 4 Bytes  |
## | ` (over 4 billion)                      4_294_967_295` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                          -9_223_372_036_854_775_808` | [I64] | 8 Bytes  |
## | `                           9_223_372_036_854_775_807` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                                   0` | [U64] | 8 Bytes  |
## | ` (over 18 quintillion)    18_446_744_073_709_551_615` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `-170_141_183_460_469_231_731_687_303_715_884_105_728` | [I128]| 16 Bytes |
## | ` 170_141_183_460_469_231_731_687_303_715_884_105_727` |       |          |
## |--------------------------------------------------------|-------|----------|
## | ` (over 340 undecillion)                            0` | [U128]| 16 Bytes |
## | ` 340_282_366_920_938_463_463_374_607_431_768_211_455` |       |          |
##
## Roc also has one variable-size integer type: [Nat]. The size of [Nat] is equal
## to the size of a memory address, which varies by system. For example, when
## compiling for a 64-bit system, [Nat] is the same as [U64]. When compiling for a
## 32-bit system, it's the same as [U32].
##
## A common use for [Nat] is to store the length ("len" for short) of a
## collection like a [List]. 64-bit systems can represent longer
## lists in memory than 32-bit systems can, which is why the length of a list
## is represented as a [Nat] in Roc.
##
## If any operation would result in an [Int] that is either too big
## or too small to fit in that range (e.g. calling `Num.maxI32 + 1`),
## then the operation will *overflow*. When an overflow occurs, the program will crash.
##
## As such, it's very important to design your code not to exceed these bounds!
## If you need to do math outside these bounds, consider using a larger numeric size.
Int range : Num (Integer range)

## A fixed-size number with a fractional component.
##
## Roc fractions come in two flavors: fixed-point base-10 and floating-point base-2.
##
## * [Dec] is a 128-bit [fixed-point](https://en.wikipedia.org/wiki/Fixed-point_arithmetic) base-10 number. It's a great default choice, especially when precision is important - for example when representing currency. With [Dec], 0.1 + 0.2 returns 0.3.
## * [F64] and [F32] are [floating-point](https://en.wikipedia.org/wiki/Floating-point_arithmetic) base-2 numbers. They sacrifice precision for lower memory usage and improved performance on some operations. This makes them a good fit for representing graphical coordinates. With [F64], 0.1 + 0.2 returns 0.3000000000000000444089209850062616169452667236328125.
##
## If you don't specify a type, Roc will default to using [Dec] because it's
## the least error-prone overall. For example, suppose you write this:
##
##     wasItPrecise = 0.1 + 0.2 == 0.3
##
## The value of `wasItPrecise` here will be `Bool.true`, because Roc uses [Dec]
## by default when there are no types specified.
##
## In contrast, suppose we use `f32` or `f64` for one of these numbers:
##
##     wasItPrecise = 0.1f64 + 0.2 == 0.3
##
## Here, `wasItPrecise` will be `Bool.false` because the entire calculation will have
## been done in a base-2 floating point calculation, which causes noticeable
## precision loss in this case.
##
## The floating-point numbers ([F32] and [F64]) also have three values which
## are not ordinary [finite numbers](https://en.wikipedia.org/wiki/Finite_number).
## They are:
## * ∞ ([infinity](https://en.wikipedia.org/wiki/Infinity))
## * -∞ (negative infinity)
## * *NaN* ([not a number](https://en.wikipedia.org/wiki/NaN))
##
## These values are different from ordinary numbers in that they only occur
## when a floating-point calculation encounters an error. For example:
## * Dividing a positive [F64] by `0.0` returns ∞.
## * Dividing a negative [F64] by `0.0` returns -∞.
## * Dividing a [F64] of `0.0` by `0.0` returns [*NaN*](Num.isNaN).
##
## These rules come from the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
## floating point standard. Because almost all modern processors are built to
## this standard, deviating from these rules has a significant performance
## cost! Since the most common reason to choose [F64] or [F32] over [Dec] is
## access to hardware-accelerated performance, Roc follows these rules exactly.
##
## There's no literal syntax for these error values, but you can check to see if
## you ended up with one of them by using #isNaN, #isFinite, and #isInfinite.
## Whenever a function in this module could return one of these values, that
## possibility is noted in the function's documentation.
##
## ## Performance Notes
##
## On typical modern CPUs, performance is similar between [Dec], [F64], and [F32]
## for addition and subtraction. For example, [F32] and [F64] do addition using
## a single CPU floating-point addition instruction, which typically takes a
## few clock cycles to complete. In contrast, [Dec] does addition using a few
## CPU integer arithmetic instructions, each of which typically takes only one
## clock cycle to complete. Exact numbers will vary by CPU, but they should be
## similar overall.
##
## [Dec] is significantly slower for multiplication and division. It not only
## needs to do more arithmetic instructions than [F32] and [F64] do, but also
## those instructions typically take more clock cycles to complete.
##
## With [Num.sqrt] and trigonometry functions like [Num.cos], there is
## an even bigger performance difference. [F32] and [F64] can do these in a
## single instruction, whereas [Dec] needs entire custom procedures - which use
## loops and conditionals. If you need to do performance-critical trigonometry
## or square roots, either [F64] or [F32] is probably a better choice than the
## usual default choice of [Dec], despite the precision problems they bring.
Frac range : Num (FloatingPoint range)

Signed128 := []
Signed64 := []
Signed32 := []
Signed16 := []
Signed8 := []

Unsigned128 := []
Unsigned64 := []
Unsigned32 := []
Unsigned16 := []
Unsigned8 := []

Natural := []

Integer range := range

I128 : Num (Integer Signed128)
I64 : Num (Integer Signed64)
I32 : Num (Integer Signed32)
I16 : Num (Integer Signed16)

## A signed 8-bit integer, ranging from -128 to 127
I8 : Int Signed8

U128 : Num (Integer Unsigned128)
U64 : Num (Integer Unsigned64)
U32 : Num (Integer Unsigned32)
U16 : Num (Integer Unsigned16)
U8 : Num (Integer Unsigned8)

## A [natural number](https://en.wikipedia.org/wiki/Natural_number) represented
## as a 64-bit unsigned integer on 64-bit systems, a 32-bit unsigned integer
## on 32-bit systems, and so on.
##
## This system-specific size makes it useful for certain data structure
## functions like [List.len], because the number of elements many data strucures
## can hold is also system-specific. For example, the maximum number of elements
## a [List] can hold on a 64-bit system fits in a 64-bit unsigned integer, and
## on a 32-bit system it fits in 32-bit unsigned integer. This makes [Nat] a
## good fit for [List.len] regardless of system.
Nat : Num (Integer Natural)

Decimal := []
Binary64 := []
Binary32 := []

FloatingPoint range := range

F64 : Num (FloatingPoint Binary64)
F32 : Num (FloatingPoint Binary32)

## A decimal number.
##
## [Dec] is the best default choice for representing base-10 decimal numbers
## like currency, because it is base-10 under the hood. In contrast,
## [F64] and [F32] are base-2 under the hood, which can lead to decimal
## precision loss even when doing addition and subtraction. For example, when
## using [F64], running 0.1 + 0.2 returns 0.3000000000000000444089209850062616169452667236328125,
## whereas when using [Dec], 0.1 + 0.2 returns 0.3.
##
## Under the hood, a [Dec] is an [I128], and operations on it perform
## [base-10 fixed-point arithmetic](https://en.wikipedia.org/wiki/Fixed-point_arithmetic)
## with 18 decimal places of precision.
##
## This means a [Dec] can represent whole numbers up to slightly over 170
## quintillion, along with 18 decimal places. (To be precise, it can store
## numbers betwween `-170_141_183_460_469_231_731.687303715884105728`
## and `170_141_183_460_469_231_731.687303715884105727`.) Why 18
## decimal places? It's the highest number of decimal places where you can still
## convert any [U64] to a [Dec] without losing information.
##
## There are some use cases where [F64] and [F32] can be better choices than [Dec]
## despite their precision issues. For example, in graphical applications they
## can be a better choice for representing coordinates because they take up
## less memory, certain relevant calculations run faster (see performance
## details, below), and decimal precision loss isn't as big a concern when
## dealing with screen coordinates as it is when dealing with currency.
##
## ## Performance
##
## [Dec] typically takes slightly less time than [F64] to perform addition and
## subtraction, but 10-20 times longer to perform multiplication and division.
## [sqrt] and trigonometry are massively slower with [Dec] than with [F64].
Dec : Num (FloatingPoint Decimal)

# ------- Functions
## Convert a number to a [Str].
##
## This is the same as calling `Num.format {}` - so for more details on
## exact formatting, see `Num.format`.
##
## >>> Num.toStr 42
##
## Only [Frac] values will include a decimal point, and they will always include one.
##
## >>> Num.toStr 4.2
##
## >>> Num.toStr 4.0
##
## When this function is given a non-[finite](Num.isFinite)
## [F64] or [F32] value, the returned string will be `"NaN"`, `"∞"`, or `"-∞"`.
##
## To get strings in hexadecimal, octal, or binary format, use `Num.format`.
toStr : Num * -> Str
intCast : Int a -> Int b

bytesToU16Lowlevel : List U8, Nat -> U16
bytesToU32Lowlevel : List U8, Nat -> U32

bytesToU16 : List U8, Nat -> Result U16 [OutOfBounds]
bytesToU16 = \bytes, index ->
    # we need at least 1 more byte
    offset = 1

    if index + offset < List.len bytes then
        Ok (bytesToU16Lowlevel bytes index)
    else
        Err OutOfBounds

bytesToU32 : List U8, Nat -> Result U32 [OutOfBounds]
bytesToU32 = \bytes, index ->
    # we need at least 3 more bytes
    offset = 3

    if index + offset < List.len bytes then
        Ok (bytesToU32Lowlevel bytes index)
    else
        Err OutOfBounds

compare : Num a, Num a -> [LT, EQ, GT]

## Returns `Bool.true` if the first number is less than the second.
##
## `a < b` is shorthand for `Num.isLt a b`.
##
## If either argument is [*NaN*](Num.isNaN), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
##
## >>> 5
## >>>     |> Num.isLt 6
isLt : Num a, Num a -> Bool

## Returns `Bool.true` if the first number is greater than the second.
##
## `a > b` is shorthand for `Num.isGt a b`.
##
## If either argument is [*NaN*](Num.isNaN), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
##
## >>> 6
## >>>     |> Num.isGt 5
isGt : Num a, Num a -> Bool

## Returns `Bool.true` if the first number is less than or equal to the second.
##
## `a <= b` is shorthand for `Num.isLte a b`.
##
## If either argument is [*NaN*](Num.isNaN), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
isLte : Num a, Num a -> Bool

## Returns `Bool.true` if the first number is greater than or equal to the second.
##
## `a >= b` is shorthand for `Num.isGte a b`.
##
## If either argument is [*NaN*](Num.isNaN), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
isGte : Num a, Num a -> Bool

## Returns `Bool.true` if the number is `0`, and `Bool.false` otherwise.
isZero : Num a -> Bool
isZero = \x -> x == 0

## A number is even if dividing it by 2 gives a remainder of 0.
##
## Examples of even numbers: 0, 2, 4, 6, 8, -2, -4, -6, -8
isEven : Int a -> Bool
isEven = \x -> Num.isMultipleOf x 2

## A number is odd if dividing it by 2 gives a remainder of 1.
##
## Examples of odd numbers: 1, 3, 5, 7, -1, -3, -5, -7
isOdd : Int a -> Bool
isOdd = \x -> Bool.not (Num.isMultipleOf x 2)

## Positive numbers are greater than `0`.
isPositive : Num a -> Bool
isPositive = \x -> x > 0

## Negative numbers are less than `0`.
isNegative : Num a -> Bool
isNegative = \x -> x < 0

toFrac : Num * -> Frac *

## Return the absolute value of the number.
##
## * For a positive number, returns the same number.
## * For a negative number, returns the same number except positive.
## * For zero, returns zero.
##
## >>> Num.abs 4
##
## >>> Num.abs -2.5
##
## >>> Num.abs 0
##
## >>> Num.abs 0.0
##
## This is safe to use with any [Frac], but it can cause overflow when used with certain [Int] values.
##
## For example, calling #Num.abs on the lowest value of a signed integer (such as [Num.minI64] or [Num.minI32]) will cause overflow.
## This is because, for any given size of signed integer (32-bit, 64-bit, etc.) its negated lowest value turns out to be 1 higher than
## the highest value it can represent. (For this reason, calling [Num.neg] on the lowest signed value will also cause overflow.)
##
## Calling this on an unsigned integer (like [U32] or [U64]) never does anything.
abs : Num a -> Num a

## Return a negative number when given a positive one, and vice versa.
##
## >>> Num.neg 5
##
## >>> Num.neg -2.5
##
## >>> Num.neg 0
##
## >>> Num.neg 0.0
##
## This is safe to use with any [Frac], but it can cause overflow when used with certain [Int] values.
##
## For example, calling #Num.neg on the lowest value of a signed integer (such as [Num.minI64] or [Num.minI32]) will cause overflow.
## This is because, for any given size of signed integer (32-bit, 64-bit, etc.) its negated lowest value turns out to be 1 higher than
## the highest value it can represent. (For this reason, calling #Num.abs on the lowest signed value will also cause overflow.)
##
## Additionally, calling #Num.neg on any unsigned integer (such as any [U64] or [U32] value) other than zero will cause overflow.
##
## (It will never crash when given a [Frac], however, because of how floating point numbers represent positive and negative numbers.)
neg : Num a -> Num a

## Add two numbers of the same type.
##
## (To add an [Int] and a [Frac], first convert one so that they both have the same type. There are functions in this module that can convert both [Int] to [Frac] and the other way around.)
##
## `a + b` is shorthand for `Num.add a b`.
##
## >>> 5 + 7
##
## >>> Num.add 5 7
##
## `Num.add` can be convenient in pipelines.
##
## >>> Frac.pi
## >>>     |> Num.add 1.0
##
## If the answer to this operation can't fit in the return value (e.g. an
## [I8] answer that's higher than 127 or lower than -128), the result is an
## *overflow*. For [F64] and [F32], overflow results in an answer of either
## ∞ or -∞. For all other number types, overflow results in a panic.
add : Num a, Num a -> Num a

## Subtract two numbers of the same type.
##
## (To subtract an [Int] and a [Frac], first convert one so that they both have the same type. There are functions in this module that can convert both [Int] to [Frac] and the other way around.)
##
## `a - b` is shorthand for `Num.sub a b`.
##
## >>> 7 - 5
##
## >>> Num.sub 7 5
##
## `Num.sub` can be convenient in pipelines.
##
## >>> Frac.pi
## >>>     |> Num.sub 2.0
##
## If the answer to this operation can't fit in the return value (e.g. an
## [I8] answer that's higher than 127 or lower than -128), the result is an
## *overflow*. For [F64] and [F32], overflow results in an answer of either
## ∞ or -∞. For all other number types, overflow results in a panic.
sub : Num a, Num a -> Num a

## Multiply two numbers of the same type.
##
## (To multiply an [Int] and a [Frac], first convert one so that they both have the same type. There are functions in this module that can convert both [Int] to [Frac] and the other way around.)
##
## `a * b` is shorthand for `Num.mul a b`.
##
## >>> 5 * 7
##
## >>> Num.mul 5 7
##
## `Num.mul` can be convenient in pipelines.
##
## >>> Frac.pi
## >>>     |> Num.mul 2.0
##
## If the answer to this operation can't fit in the return value (e.g. an
## [I8] answer that's higher than 127 or lower than -128), the result is an
## *overflow*. For [F64] and [F32], overflow results in an answer of either
## ∞ or -∞. For all other number types, overflow results in a panic.
mul : Num a, Num a -> Num a

sin : Frac a -> Frac a
cos : Frac a -> Frac a

tan : Frac a -> Frac a
tan = \x ->
    # `tan` is not available as an intrinsic in LLVM
    Num.div (Num.sin x) (Num.cos x)

asin : Frac a -> Frac a
acos : Frac a -> Frac a
atan : Frac a -> Frac a

## Returns an approximation of the absolute value of a [Frac]'s square root.
##
## The square root of a negative number is an irrational number, and [Frac] only
## supports rational numbers. As such, you should make sure never to pass this
## function a negative number! Calling [sqrt] on a negative [Dec] will cause a panic.
##
## Calling [sqrt] on [F32] and [F64] values follows these rules:
## * Passing a negative [F64] or [F32] returns [*NaN*](Num.isNaN).
## * Passing [*NaN*](Num.isNaN) or -∞ also returns [*NaN*](Num.isNaN).
## * Passing ∞ returns ∞.
##
## > These rules come from the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
## > floating point standard. Because almost all modern processors are built to
## > this standard, deviating from these rules has a significant performance
## > cost! Since the most common reason to choose [F64] or [F32] over [Dec] is
## > access to hardware-accelerated performance, Roc follows these rules exactly.
##
## >>> Num.sqrt 4.0
##
## >>> Num.sqrt 1.5
##
## >>> Num.sqrt 0.0
##
## >>> Num.sqrt -4.0f64
sqrt : Frac a -> Frac a

sqrtChecked : Frac a -> Result (Frac a) [SqrtOfNegative]*
sqrtChecked = \x ->
    if x < 0.0 then
        Err SqrtOfNegative
    else
        Ok (Num.sqrt x)

log : Frac a -> Frac a

logChecked : Frac a -> Result (Frac a) [LogNeedsPositive]*
logChecked = \x ->
    if x <= 0.0 then
        Err LogNeedsPositive
    else
        Ok (Num.log x)

## Divide one [Frac] by another.
##
## `a / b` is shorthand for `Num.div a b`.
##
## [Division by zero is undefined in mathematics](https://en.wikipedia.org/wiki/Division_by_zero).
## As such, you should make sure never to pass zero as the denomaintor to this function!
## Calling [div] on a [Dec] denominator of zero will cause a panic.
##
## Calling [div] on [F32] and [F64] values follows these rules:
## * Dividing a positive [F64] or [F32] by zero returns ∞.
## * Dividing a negative [F64] or [F32] by zero returns -∞.
## * Dividing a zero [F64] or [F32] by zero returns [*NaN*](Num.isNaN).
##
## > These rules come from the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
## > floating point standard. Because almost all modern processors are built to
## > this standard, deviating from these rules has a significant performance
## > cost! Since the most common reason to choose [F64] or [F32] over [Dec] is
## > access to hardware-accelerated performance, Roc follows these rules exactly.
##
## To divide an [Int] and a [Frac], first convert the [Int] to a [Frac] using
## one of the functions in this module like #toDec.
##
## >>> 5.0 / 7.0
##
## >>> Num.div 5 7
##
## `Num.div` can be convenient in pipelines.
##
## >>> Num.pi
## >>>     |> Num.div 2.0
div : Frac a, Frac a -> Frac a

divChecked : Frac a, Frac a -> Result (Frac a) [DivByZero]*
divChecked = \a, b ->
    if b == 0 then
        Err DivByZero
    else
        Ok (Num.div a b)

divCeil : Int a, Int a -> Int a

divCeilChecked : Int a, Int a -> Result (Int a) [DivByZero]*
divCeilChecked = \a, b ->
    if b == 0 then
        Err DivByZero
    else
        Ok (Num.divCeil a b)

## Divide two integers, truncating the result towards zero.
##
## `a // b` is shorthand for `Num.divTrunc a b`.
##
## Division by zero is undefined in mathematics. As such, you should make
## sure never to pass zero as the denomaintor to this function! If you do,
## it will crash.
##
## >>> 5 // 7
##
## >>> Num.divTrunc 5 7
##
## >>> 8 // -3
##
## >>> Num.divTrunc 8 -3
##
divTrunc : Int a, Int a -> Int a

divTruncChecked : Int a, Int a -> Result (Int a) [DivByZero]*
divTruncChecked = \a, b ->
    if b == 0 then
        Err DivByZero
    else
        Ok (Num.divTrunc a b)

## Obtain the remainder (truncating modulo) from the division of two integers.
##
## `a % b` is shorthand for `Num.rem a b`.
##
## >>> 5 % 7
##
## >>> Num.rem 5 7
##
## >>> -8 % -3
##
## >>> Num.rem -8 -3
rem : Int a, Int a -> Int a

remChecked : Int a, Int a -> Result (Int a) [DivByZero]*
remChecked = \a, b ->
    if b == 0 then
        Err DivByZero
    else
        Ok (Num.rem a b)

isMultipleOf : Int a, Int a -> Bool

bitwiseAnd : Int a, Int a -> Int a
bitwiseXor : Int a, Int a -> Int a
bitwiseOr : Int a, Int a -> Int a

## Bitwise left shift of a number by another
##
## The least significant bits always become 0. This means that shifting left is
## like multiplying by factors of two for unsigned integers.
##
## >>> shiftLeftBy 0b0000_0011 2 == 0b0000_1100
##
## >>> 0b0000_0101 |> shiftLeftBy 2 == 0b0000_1100
##
## In some languages `shiftLeftBy` is implemented as a binary operator `<<`.
shiftLeftBy : Int a, Int a -> Int a

## Bitwise arithmetic shift of a number by another
##
## The most significant bits are copied from the current.
##
## >>> shiftRightBy 0b0000_0011 2 == 0b0000_1100
##
## >>> 0b0001_0100 |> shiftRightBy 2 == 0b0000_0101
##
## >>> 0b1001_0000 |> shiftRightBy 2 == 0b1110_0100
##
## In some languages `shiftRightBy` is implemented as a binary operator `>>>`.
shiftRightBy : Int a, Int a -> Int a

## Bitwise logical right shift of a number by another
##
## The most significant bits always become 0. This means that shifting left is
## like dividing by factors of two for unsigned integers.
##
## >>> shiftRightBy 0b0010_1000 2 == 0b0000_1010
##
## >>> 0b0010_1000 |> shiftRightBy 2 == 0b0000_1010
##
## >>> 0b1001_0000 |> shiftRightBy 2 == 0b0010_0100
##
## In some languages `shiftRightBy` is implemented as a binary operator `>>`.
shiftRightZfBy : Int a, Int a -> Int a

## Round off the given fraction to the nearest integer.
round : Frac * -> Int *
floor : Frac * -> Int *
ceiling : Frac * -> Int *

## Raises a [Frac] to the power of another [Frac].
##
## For an [Int] alternative to this function, see [Num.powInt]
pow : Frac a, Frac a -> Frac a

## Raises an integer to the power of another, by multiplying the integer by
## itself the given number of times.
##
## This process is known as [exponentiation by squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring).
##
## For a [Frac] alternative to this function, which supports negative exponents,
## see #Num.exp.
##
## >>> Num.exp 5 0
##
## >>> Num.exp 5 1
##
## >>> Num.exp 5 2
##
## >>> Num.exp 5 6
##
## ## Performance Notes
##
## Be careful! It is very easy for this function to produce an answer
## so large it causes an overflow.
powInt : Int a, Int a -> Int a

addWrap : Int range, Int range -> Int range

## Add two numbers, clamping on the maximum representable number rather than
## overflowing.
##
## This is the same as [Num.add] except for the saturating behavior if the
## addition is to overflow.
## For example, if `x : U8` is 200 and `y : U8` is 100, `addSaturated x y` will
## yield 255, the maximum value of a `U8`.
addSaturated : Num a, Num a -> Num a

## Add two numbers and check for overflow.
##
## This is the same as [Num.add] except if the operation overflows, instead of
## panicking or returning ∞ or -∞, it will return `Err Overflow`.
addChecked : Num a, Num a -> Result (Num a) [Overflow]*
addChecked = \a, b ->
    result = addCheckedLowlevel a b

    if result.b then
        Err Overflow
    else
        Ok result.a

addCheckedLowlevel : Num a, Num a -> { b : Bool, a : Num a }

subWrap : Int range, Int range -> Int range

## Subtract two numbers, clamping on the minimum representable number rather
## than overflowing.
##
## This is the same as [Num.sub] except for the saturating behavior if the
## subtraction is to overflow.
## For example, if `x : U8` is 10 and `y : U8` is 20, `subSaturated x y` will
## yield 0, the minimum value of a `U8`.
subSaturated : Num a, Num a -> Num a

## Subtract two numbers and check for overflow.
##
## This is the same as [Num.sub] except if the operation overflows, instead of
## panicking or returning ∞ or -∞, it will return `Err Overflow`.
subChecked : Num a, Num a -> Result (Num a) [Overflow]*
subChecked = \a, b ->
    result = subCheckedLowlevel a b

    if result.b then
        Err Overflow
    else
        Ok result.a

subCheckedLowlevel : Num a, Num a -> { b : Bool, a : Num a }

mulWrap : Int range, Int range -> Int range

## Multiply two numbers, clamping on the maximum representable number rather than
## overflowing.
##
## This is the same as [Num.mul] except for the saturating behavior if the
## addition is to overflow.
mulSaturated : Num a, Num a -> Num a

## Multiply two numbers and check for overflow.
##
## This is the same as [Num.mul] except if the operation overflows, instead of
## panicking or returning ∞ or -∞, it will return `Err Overflow`.
mulChecked : Num a, Num a -> Result (Num a) [Overflow]*
mulChecked = \a, b ->
    result = mulCheckedLowlevel a b

    if result.b then
        Err Overflow
    else
        Ok result.a

mulCheckedLowlevel : Num a, Num a -> { b : Bool, a : Num a }

## The lowest number that can be stored in an [I8] without underflowing its
## available memory and crashing.
##
## For reference, this number is `-128`.
##
## Note that the positive version of this number is larger than [Num.maxI8],
## which means if you call [Num.abs] on [Num.minI8], it will overflow and crash!
minI8 : I8
minI8 = -128i8

## The highest number that can be stored in an [I8] without overflowing its
## available memory and crashing.
##
## For reference, this number is `127`.
##
## Note that this is smaller than the positive version of [Num.minI8],
## which means if you call [Num.abs] on [Num.minI8], it will overflow and crash!
maxI8 : I8
maxI8 = 127i8

## The lowest number that can be stored in a [U8] without underflowing its
## available memory and crashing.
##
## For reference, this number is zero, because [U8] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
minU8 : U8
minU8 = 0u8

## The highest number that can be stored in a [U8] without overflowing its
## available memory and crashing.
##
## For reference, this number is `255`.
maxU8 : U8
maxU8 = 255u8

## The lowest number that can be stored in an [I16] without underflowing its
## available memory and crashing.
##
## For reference, this number is `-32_768`.
##
## Note that the positive version of this number is larger than [Num.maxI16],
## which means if you call [Num.abs] on [Num.minI16], it will overflow and crash!
minI16 : I16
minI16 = -32768i16

## The highest number that can be stored in an [I16] without overflowing its
## available memory and crashing.
##
## For reference, this number is `32_767`.
##
## Note that this is smaller than the positive version of [Num.minI16],
## which means if you call [Num.abs] on [Num.minI16], it will overflow and crash!
maxI16 : I16
maxI16 = 32767i16

## The lowest number that can be stored in a [U16] without underflowing its
## available memory and crashing.
##
## For reference, this number is zero, because [U16] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
minU16 : U16
minU16 = 0u16

## The highest number that can be stored in a [U16] without overflowing its
## available memory and crashing.
##
## For reference, this number is `65_535`.
maxU16 : U16
maxU16 = 65535u16

## The lowest number that can be stored in an [I32] without underflowing its
## available memory and crashing.
##
## For reference, this number is `-2_147_483_648`.
##
## Note that the positive version of this number is larger than [Num.maxI32],
## which means if you call [Num.abs] on [Num.minI32], it will overflow and crash!
minI32 : I32
minI32 = -2147483648

## The highest number that can be stored in an [I32] without overflowing its
## available memory and crashing.
##
## For reference, this number is `2_147_483_647`,
## which is over 2 million.
##
## Note that this is smaller than the positive version of [Num.minI32],
## which means if you call [Num.abs] on [Num.minI32], it will overflow and crash!
maxI32 : I32
maxI32 = 2147483647

## The lowest number that can be stored in a [U32] without underflowing its
## available memory and crashing.
##
## For reference, this number is zero, because [U32] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
minU32 : U32
minU32 = 0

## The highest number that can be stored in a [U32] without overflowing its
## available memory and crashing.
##
## For reference, this number is `4_294_967_295`.
maxU32 : U32
maxU32 = 4294967295

## The lowest number that can be stored in an [I64] without underflowing its
## available memory and crashing.
##
## For reference, this number is `-9_223_372_036_854_775_808`,
## which is under 9 quintillion.
##
## Note that the positive version of this number is larger than [Num.maxI64],
## which means if you call [Num.abs] on [Num.minI64], it will overflow and crash!
minI64 : I64
minI64 = -9223372036854775808

## The highest number that can be stored in an [I64] without overflowing its
## available memory and crashing.
##
## For reference, this number is `9_223_372_036_854_775_807`,
## which is over 9 quintillion.
##
## Note that this is smaller than the positive version of [Num.minI64],
## which means if you call [Num.abs] on [Num.minI64], it will overflow and crash!
maxI64 : I64
maxI64 = 9223372036854775807

## The lowest number that can be stored in a [U64] without underflowing its
## available memory and crashing.
##
## For reference, this number is zero, because [U64] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
minU64 : U64
minU64 = 0

## The highest number that can be stored in a [U64] without overflowing its
## available memory and crashing.
##
## For reference, this number is `18_446_744_073_709_551_615`,
## which is over 18 quintillion.
maxU64 : U64
maxU64 = 18446744073709551615

## The lowest number that can be stored in an [I128] without underflowing its
## available memory and crashing.
##
## For reference, this number is `-170_141_183_460_469_231_731_687_303_715_884_105_728`.
## which is under 170 undecillion.
##
## Note that the positive version of this number is larger than [Num.maxI128],
## which means if you call [Num.abs] on [Num.minI128], it will overflow and crash!
minI128 : I128
minI128 = -170141183460469231731687303715884105728

## The highest number that can be stored in an [I128] without overflowing its
## available memory and crashing.
##
## For reference, this number is `170_141_183_460_469_231_731_687_303_715_884_105_727`,
## which is over 170 undecillion.
##
## Note that this is smaller than the positive version of [Num.minI128],
## which means if you call [Num.abs] on [Num.minI128], it will overflow and crash!
maxI128 : I128
maxI128 = 170141183460469231731687303715884105727

## The lowest number that can be stored in a [U128] without underflowing its
## available memory and crashing.
##
## For reference, this number is zero, because [U128] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
minU128 : U128
minU128 = 0

## The highest number that can be stored in a [U128] without overflowing its
## available memory and crashing.
##
## For reference, this number is `340_282_366_920_938_463_463_374_607_431_768_211_455`,
## which is over 340 undecillion.
maxU128 : U128
maxU128 = 340282366920938463463374607431768211455

minF32 : F32
minF32 = -3.40282347e38

maxF32 : F32
maxF32 = 3.40282347e38

minF64 : F64
minF64 = -1.7976931348623157e308

maxF64 : F64
maxF64 = 1.7976931348623157e308

## Converts an [Int] to an [I8]. If the given number can't be precisely represented in an [I8],
## the returned number may be different from the given number.
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

## Convert an [Int] to a [Nat]. If the given number doesn't fit in [Nat], it will be truncated.
## Since #Nat has a different maximum number depending on the system you're building
## for, this may give a different answer on different systems.
##
## For example, on a 32-bit system, #Num.maxNat will return the same answer as
## [Num.maxU32]. This means that calling `Num.toNat 9_000_000_000` on a 32-bit
## system will return [Num.maxU32] instead of 9 billion, because 9 billion is
## higher than [Num.maxU32] and will not fit in a [Nat] on a 32-bit system.
##
## However, calling `Num.toNat 9_000_000_000` on a 64-bit system will return
## the #Nat value of 9_000_000_000. This is because on a 64-bit system, [Nat] can
## hold up to [Num.maxU64], and 9_000_000_000 is lower than [Num.maxU64].
##
## To convert a [Frac] to a [Nat], first call either #Num.round, #Num.ceil, or [Num.floor]
## on it, then call this on the resulting [Int].
toNat : Int * -> Nat

## Converts a [Num] to an [F32]. If the given number can't be precisely represented in an [F32],
## the returned number may be different from the given number.
toF32 : Num * -> F32

## Converts a [Num] to an [F64]. If the given number can't be precisely represented in an [F64],
## the returned number may be different from the given number.
toF64 : Num * -> F64

## Converts a [Int] to an [I8].
## If the given integer can't be precisely represented in an [I8], returns
## `Err OutOfBounds`.
toI8Checked : Int * -> Result I8 [OutOfBounds]*
toI16Checked : Int * -> Result I16 [OutOfBounds]*
toI32Checked : Int * -> Result I32 [OutOfBounds]*
toI64Checked : Int * -> Result I64 [OutOfBounds]*
toI128Checked : Int * -> Result I128 [OutOfBounds]*
toU8Checked : Int * -> Result U8 [OutOfBounds]*
toU16Checked : Int * -> Result U16 [OutOfBounds]*
toU32Checked : Int * -> Result U32 [OutOfBounds]*
toU64Checked : Int * -> Result U64 [OutOfBounds]*
toU128Checked : Int * -> Result U128 [OutOfBounds]*
toNatChecked : Int * -> Result Nat [OutOfBounds]*
toF32Checked : Num * -> Result F32 [OutOfBounds]*
toF64Checked : Num * -> Result F64 [OutOfBounds]*

# Special Floating-Point operations
## When given a [F64] or [F32] value, returns `Bool.false` if that value is
## [*NaN*](Num.isNaN), ∞ or -∞, and `Bool.true` otherwise.
##
## Always returns `Bool.true` when given a [Dec].
##
## This is the opposite of #isInfinite, except when given [*NaN*](Num.isNaN). Both
## #isFinite and #isInfinite return `Bool.false` for [*NaN*](Num.isNaN).
# isFinite : Frac * -> Bool
## When given a [F64] or [F32] value, returns `Bool.true` if that value is either
## ∞ or -∞, and `Bool.false` otherwise.
##
## Always returns `Bool.false` when given a [Dec].
##
## This is the opposite of #isFinite, except when given [*NaN*](Num.isNaN). Both
## #isFinite and #isInfinite return `Bool.false` for [*NaN*](Num.isNaN).
# isInfinite : Frac * -> Bool
## When given a [F64] or [F32] value, returns `Bool.true` if that value is
## *NaN* ([not a number](https://en.wikipedia.org/wiki/NaN)), and `Bool.false` otherwise.
##
## Always returns `Bool.false` when given a [Dec].
##
## >>> Num.isNaN 12.3
##
## >>> Num.isNaN (Num.pow -1 0.5)
##
## *NaN* is unusual from other numberic values in that:
## * *NaN* is not equal to any other number, even itself. [Bool.isEq] always returns `Bool.false` if either argument is *NaN*.
## * *NaN* has no ordering, so [isLt], [isLte], [isGt], and [isGte] always return `Bool.false` if either argument is *NaN*.
##
## These rules come from the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
## floating point standard. Because almost all modern processors are built to
## this standard, deviating from these rules has a significant performance
## cost! Since the most common reason to choose [F64] or [F32] over [Dec] is
## access to hardware-accelerated performance, Roc follows these rules exactly.
##
## Note that you should never put a *NaN* into a [Set], or use it as the key in
## a [Dict]. The result is entries that can never be removed from those
## collections! See the documentation for [Set.insert] and [Dict.insert] for details.
# isNaN : Frac * -> Bool
## Returns the higher of two numbers.
##
## If either argument is [*NaN*](Num.isNaN), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
# max : Num a, Num a -> Num a
## Returns the lower of two numbers.
##
## If either argument is [*NaN*](Num.isNaN), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
# min : Num a, Num a -> Num a
# Branchless implementation that works for all numeric types:
#
# let is_lt = arg1 < arg2;
# let is_eq = arg1 == arg2;
# return (is_lt as i8 - is_eq as i8) + 1;
#
# 1, 1 -> (0 - 1) + 1 == 0 # Eq
# 5, 1 -> (0 - 0) + 1 == 1 # Gt
# 1, 5 -> (1 - 0) + 1 == 2 # Lt
## Returns `Lt` if the first number is less than the second, `Gt` if
## the first is greater than the second, and `Eq` if they're equal.
##
## Although this can be passed to `List.sort`, you'll get better performance
## by using `List.sortAsc` or `List.sortDesc` instead.
# compare : Num a, Num a -> [Lt, Eq, Gt]
## [Endianness](https://en.wikipedia.org/wiki/Endianness)
# Endi : [Big, Little, Native]
## The `Endi` argument does not matter for [U8] and [I8], since they have
## only one byte.
# toBytes : Num *, Endi -> List U8
## when Num.parseBytes bytes Big is
##     Ok { val: f64, rest } -> ...
##     Err (ExpectedNum (Frac Binary64)) -> ...
# parseBytes : List U8, Endi -> Result { val : Num a, rest : List U8 } [ExpectedNum a]*
## when Num.fromBytes bytes Big is
##     Ok f64 -> ...
##     Err (ExpectedNum (Frac Binary64)) -> ...
# fromBytes : List U8, Endi -> Result (Num a) [ExpectedNum a]*
# Bit shifts
## [Logical bit shift](https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift) left.
##
## `a << b` is shorthand for `Num.shl a b`.
# shl : Int a, Int a -> Int a
## [Arithmetic bit shift](https://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift) left.
##
## This is called `shlWrap` because any bits shifted
## off the beginning of the number will be wrapped around to
## the end. (In contrast, #shl replaces discarded bits with zeroes.)
# shlWrap : Int a, Int a -> Int a
## [Logical bit shift](https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift) right.
##
## `a >> b` is shorthand for `Num.shr a b`.
# shr : Int a, Int a -> Int a
## [Arithmetic bit shift](https://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift) right.
##
## This is called `shrWrap` because any bits shifted
## off the end of the number will be wrapped around to
## the beginning. (In contrast, #shr replaces discarded bits with zeroes.)
# shrWrap : Int a, Int a -> Int a
# ## Convert a number into a [Str], formatted with the given options.
# ##
# ## Default options:
# ## * `base: Decimal`
# ## * `notation: Standard`
# ## * `decimalMark: HideForIntegers "."`
# ## * `decimalDigits: { min: 0, max: All }`
# ## * `minIntDigits: 1`
# ## * `wholeSep: { mark: ",", places: 3 }`
# ##
# ## ## Options
# ##
# ##
# ## ### decimalMark
# ##
# ## * `AlwaysShow` always shows the decimal mark, no matter what.
# ## * `HideForIntegers` hides the decimal mark if all the numbers after the decimal mark are 0.
# ##
# ## The [Str] included in either of these represents the mark itself.
# ##
# ## ### `decimalDigits
# ##
# ## With 0 decimal digits, the decimal mark will still be rendered if
# ## `decimalMark` is set to `AlwaysShow`.
# ##
# ## If `max` is less than `min`, then first the number will be truncated to `max`
# ## digits, and then zeroes will be added afterwards until it reaches `min` digits.
# ##
# ## >>> Num.format 1.23 { decPlaces: 0, decPointVis: AlwaysShow }
# ##
# ## ### minIntDigits
# ##
# ## If the integer portion of number is fewer than this many digits, zeroes will
# ## be added in front of it until there are at least `minWholeDigits` digits.
# ##
# ## If this is set to zero, then numbers less than 1 will begin with `"."`
# ## rather than `"0."`.
# ##
# ## ### wholeSep
# ##
# ## Examples:
# ##
# ## In some countries (e.g. USA and UK), a comma is used to separate thousands:
# ## >>> Num.format 1_000_000 { pf: Decimal, wholeSep: { mark: ",", places: 3 } }
# ##
# ## Sometimes when rendering bits, it's nice to group them into groups of 4:
# ## >>> Num.format 1_000_000 { pf: Binary, wholeSep: { mark: " ", places: 4 } }
# ##
# ## It's also common to render hexadecimal in groups of 2:
# ## >>> Num.format 1_000_000 { pf: Hexadecimal, wholeSep: { mark: " ", places: 2 } }
# format :
#     Num *,
#     {
#         base ? [Decimal, Hexadecimal, Octal, Binary],
#         notation ? [Standard, Scientific],
#         decimalMark ? [AlwaysShow Str, HideForIntegers],
#         decimalDigits ? { min : U16, max : [All, Trunc U16, Round U16, Floor U16, Ceil U16] },
#         minWholeDigits ? U16,
#         wholeSep ? { mark : Str, places : U64 }
#     }
#     -> Str
