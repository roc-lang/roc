module [
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
    Dec,
    F64,
    F32,
    Decimal,
    Binary32,
    Binary64,
    e,
    pi,
    tau,
    abs,
    abs_diff,
    neg,
    add,
    sub,
    mul,
    min,
    max,
    is_lt,
    is_lte,
    is_gt,
    is_gte,
    is_approx_eq,
    sin,
    cos,
    tan,
    atan,
    acos,
    asin,
    is_zero,
    is_even,
    is_odd,
    to_frac,
    is_positive,
    is_negative,
    is_nan,
    is_infinite,
    is_finite,
    rem,
    rem_checked,
    div,
    div_checked,
    sqrt,
    sqrt_checked,
    log,
    log_checked,
    round,
    ceiling,
    floor,
    compare,
    pow,
    pow_int,
    count_leading_zero_bits,
    count_trailing_zero_bits,
    count_one_bits,
    add_wrap,
    add_checked,
    add_saturated,
    bitwise_and,
    bitwise_xor,
    bitwise_or,
    bitwise_not,
    shift_left_by,
    shift_right_by,
    shift_right_zf_by,
    sub_wrap,
    sub_checked,
    sub_saturated,
    mul_wrap,
    mul_saturated,
    mul_checked,
    int_cast,
    div_ceil,
    div_ceil_checked,
    div_trunc,
    div_trunc_checked,
    to_str,
    is_multiple_of,
    min_i8,
    max_i8,
    min_u8,
    max_u8,
    min_i16,
    max_i16,
    min_u16,
    max_u16,
    min_i32,
    max_i32,
    min_u32,
    max_u32,
    min_i64,
    max_i64,
    min_u64,
    max_u64,
    min_i128,
    max_i128,
    min_u128,
    max_u128,
    min_f32,
    max_f32,
    min_f64,
    max_f64,
    to_i8,
    to_i8_checked,
    to_i16,
    to_i16_checked,
    to_i32,
    to_i32_checked,
    to_i64,
    to_i64_checked,
    to_i128,
    to_i128_checked,
    to_u8,
    to_u8_checked,
    to_u16,
    to_u16_checked,
    to_u32,
    to_u32_checked,
    to_u64,
    to_u64_checked,
    to_u128,
    to_u128_checked,
    to_f32,
    to_f32_checked,
    to_f64,
    to_f64_checked,
    without_decimal_point,
    with_decimal_point,
    f32_to_parts,
    f64_to_parts,
    f32_from_parts,
    f64_from_parts,
    from_bool,
    nan_f32,
    nan_f64,
    infinity_f32,
    infinity_f64,
]

import Bool exposing [Bool]
import Result exposing [Result]

## Represents a number that could be either an [Int] or a [Frac].
##
## This is useful for functions that can work on either, for example [Num.add], whose type is:
## ```roc
## add : Num a, Num a -> Num a
## ```
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
## For example, in `1 + List.len(my_list)`, the `1` has the type `Num *` at first,
## but because `List.len` returns a `U64`, the `1` ends up changing from
## `Num *` to the more specific `U64`, and the expression as a whole
## ends up having the type `U64`.
##
## Sometimes number literals don't become more specific. For example,
## the `Num.to_str` function has the type `Num * -> Str`. This means that
## when calling `Num.to_str(5 + 6)`, the expression `5 + 6`
## still has the type `Num *`. When this happens, `Num *` defaults to
## being an [I64] - so this addition expression would overflow
## if either 5 or 6 were replaced with a number big enough to cause
## addition overflow on an [I64] value.
##
## If this default of [I64] is not big enough for your purposes,
## you can add an `i128` to the end of the number literal, like so:
## ```roc
## Num.to_str(5_000_000_000i128)
## ```
## This `i128` suffix specifies that you want this number literal to be
## an [I128] instead of a `Num *`. All the other numeric types have
## suffixes just like `i128`; here are some other examples:
##
## * `215u8` is a `215` value of type [U8]
## * `76.4f32` is a `76.4` value of type [F32]
## * `123.45dec` is a `123.45` value of type [Dec]
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
## like [U8], it can store 256 numbers (still 2^8), but because it is signed,
## the range is different. Its 256 numbers range from -128 to 127.
##
## Here are some other examples:
##
## * [U16] is like [U8], except it takes up 16 bits in memory. It can store 65,536 numbers (2^16), ranging from 0 to 65,536.
## * [I16] is like [U16], except it is signed. It can still store the same 65,536 numbers (2^16), ranging from -32,768 to 32,767.
##
## This pattern continues up to [U128] and [I128].
##
## ## Performance Details
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
## You can optionally put underscores in your [Int] literals.
## They have no effect on the number's value, but can make large numbers easier to read.
## ```roc
## 1_000_000
## ```
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
## * Smaller integer sizes take up less memory. These savings rarely matter in variables and function arguments, but the sizes of integers that you use in data structures can add up. This can also affect whether those data structures fit in [cache lines](https://en.wikipedia.org/wiki/CPU_cache#Cache_performance), which can be a performance bottleneck.
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
## If any operation would result in an [Int] that is either too big
## or too small to fit in that range (e.g. calling `Num.max_i32 + 1`),
## then the operation will *overflow*. When an overflow occurs, the program will crash.
##
## As such, it's very important to design your code not to exceed these bounds!
## If you need to do math outside these bounds, consider using a larger numeric size.
Int range : Num (Integer range)

## A fixed-size number with a fractional component.
##
## Roc fractions come in two flavors: fixed-point base-10 and floating-point base-2.
##
## * [Dec] is a 128-bit [fixed-point](https://en.wikipedia.org/wiki/Fixed-point_arithmetic) base-10 number. It's a great default choice, especially when precision is important - for example when representing currency. With [Dec], `0.1 + 0.2` returns `0.3`. [Dec] has 18 decimal places of precision and a range from `-170_141_183_460_469_231_731.687303715884105728` to `170_141_183_460_469_231_731.687303715884105727`.
## * [F64] and [F32] are [floating-point](https://en.wikipedia.org/wiki/Floating-point_arithmetic) base-2 numbers. They sacrifice precision for lower memory usage and improved performance on some operations. This makes them a good fit for representing graphical coordinates. With [F64], `0.1 + 0.2` returns `0.30000000000000004`.
##
## If you don't specify a type, Roc will default to using [Dec] because it's
## the least error-prone overall. For example, suppose you write this:
## ```roc
## was_it_precise = 0.1 + 0.2 == 0.3
## ```
## The value of `was_it_precise` here will be `Bool.true`, because Roc uses [Dec]
## by default when there are no types specified.
##
## In contrast, suppose we use `f32` or `f64` for one of these numbers:
## ```roc
## was_it_precise = 0.1f64 + 0.2 == 0.3
## ```
## Here, `was_it_precise` will be `Bool.false` because the entire calculation will have
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
## * Dividing a [F64] of `0.0` by `0.0` returns [*NaN*](Num.is_nan).
##
## These rules come from the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
## floating point standard. Because almost all modern processors are built to
## this standard, deviating from these rules has a significant performance
## cost! Since the most common reason to choose [F64] or [F32] over [Dec] is
## access to hardware-accelerated performance, Roc follows these rules exactly.
##
## There's no literal syntax for these error values, but you can check to see if
## you ended up with one of them by using #is_nan, #is_finite, and #is_infinite.
## Whenever a function in this module could return one of these values, that
## possibility is noted in the function's documentation.
##
## ## Performance Details
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

Decimal := []
Binary64 := []
Binary32 := []

FloatingPoint range := range

## A 64-bit [IEEE 754 binary floating-point number](https://en.wikipedia.org/wiki/IEEE_754).
##
## [F64] represents decimal numbers less precisely than [Dec] does, but operations on it
## can be faster because CPUs have hardware-level support for [F64] but not [Dec]. There
## are other tradeoffs between the two, such as:
## * [Dec] has a fixed number of digits it can represent before the decimal point, and a fixed number it can represent after the decimal point. In contrast, [F64]'s decimal point can "float"—which conceptually means if you don't need many digits before the decimal point, you can get more digits of precision afterwards (and vice versa).
## * [Dec] represents its number internally in [base-10](https://en.wikipedia.org/wiki/Decimal), whereas [F64] uses [base-2](https://en.wikipedia.org/wiki/Binary_number). This can lead to imprecise answers like `0.1 + 0.2` returning `0.3` for [Dec] and `0.30000000000000004` for [F64]. This is not a bug; rather, it's a consequence of [F64]'s base-2 representation.
## * [Dec] always gives a precise answer (or an error), whereas [F64] can lose precision. For example, increasing a very large [F64] number (using addition, perhaps) can result in the whole number portion being incorrect. `1234567890123456789 + 100` correctly results in a number ending in `889` for `Dec`, but results in a number ending `800` in [F64] due to precision loss.
F64 : Num (FloatingPoint Binary64)

## A 32-bit [IEEE 754 binary floating-point number](https://en.wikipedia.org/wiki/IEEE_754).
##
## This works just like [F64] (see its docs for a comparison with [Dec]) except it's smaller.
## That in turn means it takes up less memory, but can store smaller numbers (and becomes imprecise
## more easily than [F64] does).
F32 : Num (FloatingPoint Binary32)

## A [decimal](https://en.wikipedia.org/wiki/Decimal) number.
##
## [Dec] is a more precise way to represent decimal numbers (like currency) than [F32] and [F64]
## are, because [Dec] is represented in memory as base-10. In contrast, [F64] and [F32]
## are [base-2](https://en.wikipedia.org/wiki/Binary_number) in memory, which can lead to decimal
## precision loss even when doing addition and subtraction. For example, when
## using [F64], `0.1 + 0.2` returns 0.30000000000000004,
## whereas when using [Dec], `0.1 + 0.2` returns 0.3.
##
## Under the hood, a [Dec] is an [I128], and operations on it perform
## [base-10 fixed-point arithmetic](https://en.wikipedia.org/wiki/Fixed-point_arithmetic)
## with 18 decimal places of precision.
##
## This means a [Dec] can represent whole numbers up to slightly over 170
## quintillion, along with 18 decimal places. (To be precise, it can store
## numbers between `-170_141_183_460_469_231_731.687303715884105728`
## and `170_141_183_460_469_231_731.687303715884105727`.) Why 18
## decimal places? It's the highest number of decimal places where you can
## still convert any [U64] to a [Dec] without losing information.
##
## There are some use cases where [F64] and [F32] can be better choices than [Dec]
## despite their issues with base-10 numbers. For example, in graphical applications
## they can be a better choice for representing coordinates because they take up
## less memory, certain relevant calculations run faster (see performance
## details, below), and base-10 generally isn't as big a concern when
## dealing with screen coordinates as it is when dealing with currency.
##
## Another scenario where [F64] can be a better choice than [Dec] is when representing
## extremely small numbers. The smallest positive [F64] that can be represented without precision
## loss is 2^(−1074), which is about 5 * 10^(-324). Here is that number next to the smallest
## [Dec] that can be represented:
##
## * Smallest [F64]: 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005
## * Smallest [Dec]: 0.000000000000000001
##
## This is because [floating-point](https://en.wikipedia.org/wiki/Floating-point_arithmetic) numbers
## like [F64] can gain more digits of precision after the `.` when they aren't using as many digits
## before the `.` - and this applies the most if the digit before `.` is `0`. So whereas [Dec] always
## has 18 digits after the `.`, the number of digits after the `.` that [F32] can [F64] can represent
## without precision loss depends on what comes before it.
##
## ## Performance Details
##
## CPUs have dedicated instructions for many [F32] and [F64] operations, but none for [Dec].
## Internally, [Dec] is represented as a 128-bit integer and uses multiple instructions to
## perform fractional operations. This gives [F32] and [F64] performance advantages
## for many operations.
##
## Here's a comparison of about how long [Dec] takes to perform a given operation compared to [F64],
## based on benchmarks on an [M1](https://en.wikipedia.org/wiki/Apple_M1) CPU:
## * [add]  0.6x
## * [sub]  0.6x
## * [mul]  15x
## * [div]  55x
## * [sin]  3.9x
## * [cos]  3.6x
## * [tan]  2.3x
## * [asin] 1.8x
## * [acos] 1.7x
## * [atan] 1.7x
##
## Keep in mind that arithmetic instructions are basically [the fastest thing a CPU does](http://norvig.com/21-days.html#answers),
## so (for example) a network request that takes 10 milliseconds to complete would go on this
## list as about 10000000x. So these performance differences might be more or less noticeable than
## the base-10 representation differences depending on the use case.
Dec : Num (FloatingPoint Decimal)

## Euler's number (e)
e : Frac *
e = 2.71828182845904523536028747135266249775724709369995

## Archimedes' constant (π)
pi : Frac *
pi = 3.14159265358979323846264338327950288419716939937510

## Circle constant (τ)
tau : Frac *
tau = 6.2831853071795864769252867665590057683943387987502

# ------- Functions
## Convert a number to a [Str].
##
## ```roc
## Num.to_str(42)
## ```
## Only [Frac] values will include a decimal point, and they will always include one.
## ```roc
## Num.to_str(4.2)
## Num.to_str(4.0)
## ```
## When this function is given a non-[finite](Num.is_finite)
## [F64] or [F32] value, the returned string will be `"NaN"`, `"∞"`, or `"-∞"`.
##
to_str : Num * -> Str

## Convert an [Int] to a new [Int] of the expected type:
##
## ```roc
## # Casts a U8 to a U16
## x : U16
## x = Num.int_cast(255u8)
## ```
##
## In the case of downsizing, information is lost:
##
## ```roc
## # returns 0, as the bits were truncated.
## x : U8
## x = Num.int_cast(256u16)
## ```
##
int_cast : Int a -> Int b

compare : Num a, Num a -> [LT, EQ, GT]

## Returns `Bool.true` if the first number is less than the second.
##
## `a < b` is shorthand for `Num.is_lt(a, b)`.
##
## If either argument is [*NaN*](Num.is_nan), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
## ```roc
## 5
##     |> Num.is_lt 6
## ```
is_lt : Num a, Num a -> Bool

## Returns `Bool.true` if the first number is greater than the second.
##
## `a > b` is shorthand for `Num.is_gt a b`.
##
## If either argument is [*NaN*](Num.is_nan), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
## ```roc
## Num.is_gt(6, 5)
## ```
is_gt : Num a, Num a -> Bool

## Returns `Bool.true` if the first number is less than or equal to the second.
##
## `a <= b` is shorthand for `Num.is_lte(a, b)`.
##
## If either argument is [*NaN*](Num.is_nan), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
is_lte : Num a, Num a -> Bool

## Returns `Bool.true` if the first number is greater than or equal to the second.
##
## `a >= b` is shorthand for `Num.is_gte(a, b)`.
##
## If either argument is [*NaN*](Num.is_nan), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
is_gte : Num a, Num a -> Bool

## Returns `Bool.true` if the first number and second number are within a specific threshold
##
## A specific relative and absolute tolerance can be provided to change the threshold
##
## This function is symmetric: `Num.is_approx_eq(a, b) == Num.is_approx_eq(b, a)`
##
## If either argument is [*NaN*](Num.is_nan), returns `Bool.false` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
is_approx_eq : Frac a, Frac a, { rtol ?? Frac a, atol ?? Frac a } -> Bool
is_approx_eq = |x, y, { rtol ?? 0.00001, atol ?? 0.00000001 }|
    eq = x <= y and x >= y
    meets_tolerance = Num.abs_diff(x, y) <= Num.max(atol, (rtol * Num.max(Num.abs(x), Num.abs(y))))
    eq or meets_tolerance

## Returns `Bool.true` if the number is `0`, and `Bool.false` otherwise.
is_zero : Num a -> Bool

## A number is even if dividing it by 2 gives a remainder of 0.
##
## Examples of even numbers: 0, 2, 4, 6, 8, -2, -4, -6, -8
is_even : Int a -> Bool
is_even = |x| Num.is_multiple_of(x, 2)

## A number is odd if dividing it by 2 gives a remainder of 1.
##
## Examples of odd numbers: 1, 3, 5, 7, -1, -3, -5, -7
is_odd : Int a -> Bool
is_odd = |x| Bool.not(Num.is_multiple_of(x, 2))

## Positive numbers are greater than `0`.
is_positive : Num a -> Bool
is_positive = |x| x > 0

## Negative numbers are less than `0`.
is_negative : Num a -> Bool
is_negative = |x| x < 0

to_frac : Num * -> Frac *

## Returns `Bool.true` if the [Frac] is not a number as defined by [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
##
## ```roc
## Num.is_nan(0 / 0)
## ```
is_nan : Frac * -> Bool

## Returns `Bool.true` if the [Frac] is positive or negative infinity as defined by [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
##
## ```roc
## Num.is_infinite(1 / 0)
##
## Num.is_infinite(-1 / 0)
## ```
is_infinite : Frac * -> Bool

## Returns `Bool.true` if the [Frac] is not an infinity as defined by [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
##
## ```roc
## Num.is_finite(42)
## ```
is_finite : Frac * -> Bool

## Returns the absolute value of the number.
##
## * For a positive number, returns the same number.
## * For a negative number, returns the same number except positive.
## * For zero, returns zero.
## ```roc
## Num.abs(4)
##
## Num.abs(-2.5)
##
## Num.abs(0)
##
## Num.abs(0.0)
## ```
## This is safe to use with any [Frac], but it can cause overflow when used with certain [Int] values.
##
## For example, calling #Num.abs on the lowest value of a signed integer (such as [Num.min_i64] or [Num.min_i32]) will cause overflow.
## This is because, for any given size of signed integer (32-bit, 64-bit, etc.) its negated lowest value turns out to be 1 higher than
## the highest value it can represent. (For this reason, calling [Num.neg] on the lowest signed value will also cause overflow.)
##
## Calling this on an unsigned integer (like [U32] or [U64]) never does anything.
abs : Num a -> Num a

## Returns the absolute difference between two numbers.
##
## ```roc
## Num.abs_diff(5, 3)
##
## Num.abs_diff(-3, 5)
##
## Num.abs_diff(3.0, 5.0)
## ```
##
## If the answer to this operation can't fit in the return value (e.g. an
## [I8] answer that's higher than 127 or lower than -128), the result is an
## *overflow*. For [F64] and [F32], overflow results in an answer of either
## ∞ or -∞. For all other number types, overflow results in a panic.
abs_diff : Num a, Num a -> Num a
abs_diff = |a, b|
    if a > b then
        a - b
    else
        b - a

## Returns a negative number when given a positive one, and vice versa.
## ```roc
## Num.neg(5)
##
## Num.neg(-2.5)
##
## Num.neg(0)
##
## Num.neg(0.0)
## ```
## !! Num.neg is not completely implemented for all types in all contexts, see github.com/roc-lang/roc/issues/6959
## You can use `\some_num -> 0 - some_num` as a workaround.
##
## This is safe to use with any [Frac], but it can cause overflow when used with certain [Int] values.
##
## For example, calling #Num.neg on the lowest value of a signed integer (such as [Num.min_i64] or [Num.min_i32]) will cause overflow.
## This is because, for any given size of signed integer (32-bit, 64-bit, etc.) its negated lowest value turns out to be 1 higher than
## the highest value it can represent. (For this reason, calling #Num.abs on the lowest signed value will also cause overflow.)
##
## Additionally, calling #Num.neg on any unsigned integer (such as any [U64] or [U32] value) other than zero will cause overflow.
##
## (It will never crash when given a [Frac], however, because of how floating point numbers represent positive and negative numbers.)
neg : Num a -> Num a

## Adds two numbers of the same type.
##
## (To add an [Int] and a [Frac], first convert one so that they both have the same type. There are functions in this module that can convert both [Int] to [Frac] and the other way around.)
##
## `a + b` is shorthand for `Num.add(a, b)`.
## ```roc
## 5 + 7
##
## Num.add(5, 7)
## ```
## `Num.add` can be convenient in pipelines.
## ```roc
## Frac.pi
##     |> Num.add(1.0)
## ```
## If the answer to this operation can't fit in the return value (e.g. an
## [I8] answer that's higher than 127 or lower than -128), the result is an
## *overflow*. For [F64] and [F32], overflow results in an answer of either
## ∞ or -∞. For all other number types, overflow results in a panic.
add : Num a, Num a -> Num a

## Subtracts two numbers of the same type.
##
## (To subtract an [Int] and a [Frac], first convert one so that they both have the same type. There are functions in this module that can convert both [Int] to [Frac] and the other way around.)
##
## `a - b` is shorthand for `Num.sub(a, b)`.
## ```roc
## 7 - 5
##
## Num.sub(7, 5)
## ```
## `Num.sub` can be convenient in pipelines.
## ```roc
## Frac.pi
##     |> Num.sub(2.0)
## ```
## If the answer to this operation can't fit in the return value (e.g. an
## [I8] answer that's higher than 127 or lower than -128), the result is an
## *overflow*. For [F64] and [F32], overflow results in an answer of either
## ∞ or -∞. For all other number types, overflow results in a panic.
sub : Num a, Num a -> Num a

## Multiplies two numbers of the same type.
##
## (To multiply an [Int] and a [Frac], first convert one so that they both have the same type. There are functions in this module that can convert both [Int] to [Frac] and the other way around.)
##
## `a * b` is shorthand for `Num.mul(a, b)`.
## ```roc
## 5 * 7
##
## Num.mul(5, 7)
## ```
##
## `Num.mul` can be convenient in pipelines.
##
## ```roc
## Frac.pi
##     |> Num.mul(2.0)
## ```
## If the answer to this operation can't fit in the return value (e.g. an
## [I8] answer that's higher than 127 or lower than -128), the result is an
## *overflow*. For [F64] and [F32], overflow results in an answer of either
## ∞ or -∞. For all other number types, overflow results in a panic.
mul : Num a, Num a -> Num a

## Obtains the smaller between two numbers of the same type.
##
## ```roc
## Num.min(100, 0)
##
## Num.min(3.0, -3.0)
## ```
min : Num a, Num a -> Num a
min = |a, b|
    if a < b then
        a
    else
        b

## Obtains the greater between two numbers of the same type.
##
## ```roc
## Num.max(100, 0)
##
## Num.max(3.0, -3.0)
## ```
max : Num a, Num a -> Num a
max = |a, b|
    if a > b then
        a
    else
        b

sin : Frac a -> Frac a
cos : Frac a -> Frac a
tan : Frac a -> Frac a

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
## * Passing a negative [F64] or [F32] returns [*NaN*](Num.is_nan).
## * Passing [*NaN*](Num.is_nan) or -∞ also returns [*NaN*](Num.is_nan).
## * Passing ∞ returns ∞.
##
## > These rules come from the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
## > floating point standard. Because almost all modern processors are built to
## > this standard, deviating from these rules has a significant performance
## > cost! Since the most common reason to choose [F64] or [F32] over [Dec] is
## > access to hardware-accelerated performance, Roc follows these rules exactly.
## ```roc
## Num.sqrt(4.0)
##
## Num.sqrt(1.5)
##
## Num.sqrt(0.0)
##
## Num.sqrt(-4.0f64)
## ```
sqrt : Frac a -> Frac a

sqrt_checked : Frac a -> Result (Frac a) [SqrtOfNegative]
sqrt_checked = |x|
    if x < 0.0 then
        Err(SqrtOfNegative)
    else
        Ok(Num.sqrt(x))

## Natural logarithm
log : Frac a -> Frac a

log_checked : Frac a -> Result (Frac a) [LogNeedsPositive]
log_checked = |x|
    if x <= 0.0 then
        Err(LogNeedsPositive)
    else
        Ok(Num.log(x))

## Divides one [Frac] by another.
##
## `a / b` is shorthand for `Num.div(a, b)`.
##
## [Division by zero is undefined in mathematics](https://en.wikipedia.org/wiki/Division_by_zero).
## As such, you should make sure never to pass zero as the denominator to this function!
## Calling [div] on a [Dec] denominator of zero will cause a panic.
##
## Calling [div] on [F32] and [F64] values follows these rules:
## * Dividing a positive [F64] or [F32] by zero returns ∞.
## * Dividing a negative [F64] or [F32] by zero returns -∞.
## * Dividing a zero [F64] or [F32] by zero returns [*NaN*](Num.is_nan).
##
## > These rules come from the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
## > floating point standard. Because almost all modern processors are built to
## > this standard, deviating from these rules has a significant performance
## > cost! Since the most common reason to choose [F64] or [F32] over [Dec] is
## > access to hardware-accelerated performance, Roc follows these rules exactly.
##
## To divide an [Int] and a [Frac], first convert the [Int] to a [Frac] using
## one of the functions in this module like #to_dec.
## ```roc
## 5.0 / 7.0
##
## Num.div(5, 7)
## ```
## `Num.div` can be convenient in pipelines.
## ```roc
## Num.pi
##     |> Num.div (2.0)
## ```
div : Frac a, Frac a -> Frac a

div_checked : Frac a, Frac a -> Result (Frac a) [DivByZero]
div_checked = |a, b|
    if Num.is_zero(b) then
        Err(DivByZero)
    else
        Ok(Num.div(a, b))

div_ceil : Int a, Int a -> Int a

div_ceil_checked : Int a, Int a -> Result (Int a) [DivByZero]
div_ceil_checked = |a, b|
    if Num.is_zero(b) then
        Err(DivByZero)
    else
        Ok(Num.div_ceil(a, b))

## Divides two integers, truncating the result towards zero.
##
## `a // b` is shorthand for `Num.div_trunc(a, b)`.
##
## Division by zero is undefined in mathematics. As such, you should make
## sure never to pass zero as the denominator to this function! If you do,
## it will crash.
## ```roc
## 5 // 7
##
## Num.div_trunc(5, 7)
##
## 8 // -3
##
## Num.div_trunc(8, -3)
## ```
div_trunc : Int a, Int a -> Int a
div_trunc = |a, b|
    if Num.is_zero(b) then
        crash("Integer division by 0!")
    else
        Num.div_trunc_unchecked(a, b)

div_trunc_checked : Int a, Int a -> Result (Int a) [DivByZero]
div_trunc_checked = |a, b|
    if Num.is_zero(b) then
        Err(DivByZero)
    else
        Ok(Num.div_trunc_unchecked(a, b))

## traps (hardware fault) when given zero as the second argument.
div_trunc_unchecked : Int a, Int a -> Int a

## Obtains the remainder (truncating modulo) from the division of two integers.
##
## `a % b` is shorthand for `Num.rem(a, b)`.
## ```roc
## 5 % 7
##
## Num.rem(5, 7)
##
## -8 % -3
##
## Num.rem(-8, -3)
## ```
rem : Int a, Int a -> Int a
rem = |a, b|
    if Num.is_zero(b) then
        crash("Integer division by 0!")
    else
        Num.rem_unchecked(a, b)

rem_checked : Int a, Int a -> Result (Int a) [DivByZero]
rem_checked = |a, b|
    if Num.is_zero(b) then
        Err(DivByZero)
    else
        Ok(Num.rem_unchecked(a, b))

## traps (hardware fault) when given zero as the second argument.
rem_unchecked : Int a, Int a -> Int a

is_multiple_of : Int a, Int a -> Bool

## Does a "bitwise and". Each bit of the output is 1 if the corresponding bit
## of x AND of y is 1, otherwise it's 0.
bitwise_and : Int a, Int a -> Int a

## Does a "bitwise exclusive or". Each bit of the output is the same as the
## corresponding bit in x if that bit in y is 0, and it's the complement of
## the bit in x if that bit in y is 1.
bitwise_xor : Int a, Int a -> Int a

## Does a "bitwise or". Each bit of the output is 0 if the corresponding bit
## of x OR of y is 0, otherwise it's 1.
bitwise_or : Int a, Int a -> Int a

## Returns the complement of x - the number you get by switching each 1 for a
## 0 and each 0 for a 1. This is the same as -x - 1.
bitwise_not : Int a -> Int a
bitwise_not = |n|
    bitwise_xor(n, sub_wrap(0, 1))

## Bitwise left shift of a number by another
##
## The least significant bits always become 0. This means that shifting left is
## like multiplying by factors of two for unsigned integers.
## ```roc
## shift_left_by(0b0000_0011, 2) == 0b0000_1100
##
## 0b0000_0101 |> shift_left_by(2) == 0b0001_0100
## ```
## In some languages `shift_left_by` is implemented as a binary operator `<<`.
shift_left_by : Int a, U8 -> Int a

## Bitwise arithmetic shift of a number by another
##
## The most significant bits are copied from the current.
## ```roc
## shift_right_by(0b0000_1100, 2) == 0b0000_0011
##
## 0b0001_0100 |> shift_right_by(2) == 0b0000_0101
##
## 0b1001_0000 |> shift_right_by(2) == 0b1110_0100
## ```
## In some languages `shift_right_by` is implemented as a binary operator `>>>`.
shift_right_by : Int a, U8 -> Int a

## Bitwise logical right shift of a number by another
##
## The most significant bits always become 0. This means that shifting right is
## like dividing by factors of two for unsigned integers.
## ```roc
## shift_right_zf_by(0b0010_1000, 2) == 0b0000_1010
##
## 0b0010_1000 |> shift_right_zf_by(2) == 0b0000_1010
##
## 0b1001_0000 |> shift_right_zf_by(2) == 0b0010_0100
## ```
## In some languages `shift_right_zf_by` is implemented as a binary operator `>>`.
shift_right_zf_by : Int a, U8 -> Int a

## Round off the given fraction to the nearest integer.
round : Frac * -> Int *
floor : Frac * -> Int *
ceiling : Frac * -> Int *

## Raises a [Frac] to the power of another [Frac].
##
## For an [Int] alternative to this function, see [Num.pow_int]
pow : Frac a, Frac a -> Frac a

## Raises an integer to the power of another, by multiplying the integer by
## itself the given number of times.
##
## This process is known as [exponentiation by squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring).
##
## For a [Frac] alternative to this function, which supports negative exponents,
## see #Num.pow.
##
## ## Warning
##
## It is very easy for this function to produce an answer
## so large it causes an overflow.
pow_int : Int a, Int a -> Int a

## Counts the number of most-significant (leading in a big-Endian sense) zeroes in an integer.
##
## ```roc
## Num.count_leading_zero_bits(0b0001_1100u8)
##
## 3
##
## Num.count_leading_zero_bits(0b0000_0000u8)
##
## 8
## ```
count_leading_zero_bits : Int a -> U8

## Counts the number of least-significant (trailing in a big-Endian sense) zeroes in an integer.
##
## ```roc
## Num.count_trailing_zero_bits(0b0001_1100u8)
##
## 2
##
## Num.count_trailing_zero_bits(0b0000_0000u8)
##
## 8
## ```
count_trailing_zero_bits : Int a -> U8

## Counts the number of set bits in an integer.
##
## ```roc
## Num.count_one_bits(0b0001_1100u8)
##
## 3
##
## Num.count_one_bits(0b0000_0000u8)
##
## 0
## ```
count_one_bits : Int a -> U8

add_wrap : Int range, Int range -> Int range

## Adds two numbers, clamping on the maximum representable number rather than
## overflowing.
##
## This is the same as [Num.add] except for the saturating behavior if the
## addition is to overflow.
## For example, if `x : U8` is 200 and `y : U8` is 100, `add_saturated x y` will
## yield 255, the maximum value of a `U8`.
add_saturated : Num a, Num a -> Num a

## Adds two numbers and checks for overflow.
##
## This is the same as [Num.add] except if the operation overflows, instead of
## panicking or returning ∞ or -∞, it will return `Err Overflow`.
add_checked : Num a, Num a -> Result (Num a) [Overflow]
add_checked = |a, b|
    result = add_checked_lowlevel(a, b)

    if result.b then
        Err(Overflow)
    else
        Ok(result.a)

add_checked_lowlevel : Num a, Num a -> { b : Bool, a : Num a }

sub_wrap : Int range, Int range -> Int range

## Subtracts two numbers, clamping on the minimum representable number rather
## than overflowing.
##
## This is the same as [Num.sub] except for the saturating behavior if the
## subtraction is to overflow.
## For example, if `x : U8` is 10 and `y : U8` is 20, `sub_saturated x y` will
## yield 0, the minimum value of a `U8`.
sub_saturated : Num a, Num a -> Num a

## Subtracts two numbers and checks for overflow.
##
## This is the same as [Num.sub] except if the operation overflows, instead of
## panicking or returning ∞ or -∞, it will return `Err Overflow`.
sub_checked : Num a, Num a -> Result (Num a) [Overflow]
sub_checked = |a, b|
    result = sub_checked_lowlevel(a, b)

    if result.b then
        Err(Overflow)
    else
        Ok(result.a)

sub_checked_lowlevel : Num a, Num a -> { b : Bool, a : Num a }

mul_wrap : Int range, Int range -> Int range

## Multiplies two numbers, clamping on the maximum representable number rather than
## overflowing.
##
## This is the same as [Num.mul] except for the saturating behavior if the
## addition is to overflow.
mul_saturated : Num a, Num a -> Num a

## Multiplies two numbers and checks for overflow.
##
## This is the same as [Num.mul] except if the operation overflows, instead of
## panicking or returning ∞ or -∞, it will return `Err Overflow`.
mul_checked : Num a, Num a -> Result (Num a) [Overflow]
mul_checked = |a, b|
    result = mul_checked_lowlevel(a, b)

    if result.b then
        Err(Overflow)
    else
        Ok(result.a)

mul_checked_lowlevel : Num a, Num a -> { b : Bool, a : Num a }

## Returns the lowest number that can be stored in an [I8] without underflowing
## its available memory and crashing.
##
## For reference, this number is `-128`.
##
## Note that the positive version of this number is larger than [Num.max_i8],
## which means if you call [Num.abs] on [Num.min_i8], it will overflow and crash!
min_i8 : I8
min_i8 = -128i8

## Returns the highest number that can be stored in an [I8] without overflowing
## its available memory and crashing.
##
## For reference, this number is `127`.
##
## Note that this is smaller than the positive version of [Num.min_i8],
## which means if you call [Num.abs] on [Num.min_i8], it will overflow and crash!
max_i8 : I8
max_i8 = 127i8

## Returns the lowest number that can be stored in a [U8] without underflowing
## its available memory and crashing.
##
## For reference, this number is zero, because [U8] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
min_u8 : U8
min_u8 = 0u8

## Returns the highest number that can be stored in a [U8] without overflowing
## its available memory and crashing.
##
## For reference, this number is `255`.
max_u8 : U8
max_u8 = 255u8

## Returns the lowest number that can be stored in an [I16] without underflowing
## its available memory and crashing.
##
## For reference, this number is `-32_768`.
##
## Note that the positive version of this number is larger than [Num.max_i16],
## which means if you call [Num.abs] on [Num.min_i16], it will overflow and crash!
min_i16 : I16
min_i16 = -32768i16

## Returns the highest number that can be stored in an [I16] without overflowing
## its available memory and crashing.
##
## For reference, this number is `32_767`.
##
## Note that this is smaller than the positive version of [Num.min_i16],
## which means if you call [Num.abs] on [Num.min_i16], it will overflow and crash!
max_i16 : I16
max_i16 = 32767i16

## Returns the lowest number that can be stored in a [U16] without underflowing
## its available memory and crashing.
##
## For reference, this number is zero, because [U16] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
min_u16 : U16
min_u16 = 0u16

## Returns the highest number that can be stored in a [U16] without overflowing
## its available memory and crashing.
##
## For reference, this number is `65_535`.
max_u16 : U16
max_u16 = 65535u16

## Returns the lowest number that can be stored in an [I32] without underflowing
## its available memory and crashing.
##
## For reference, this number is `-2_147_483_648`.
##
## Note that the positive version of this number is larger than [Num.max_i32],
## which means if you call [Num.abs] on [Num.min_i32], it will overflow and crash!
min_i32 : I32
min_i32 = -2147483648

## Returns the highest number that can be stored in an [I32] without overflowing
## its available memory and crashing.
##
## For reference, this number is `2_147_483_647`,
## which is over 2 million.
##
## Note that this is smaller than the positive version of [Num.min_i32],
## which means if you call [Num.abs] on [Num.min_i32], it will overflow and crash!
max_i32 : I32
max_i32 = 2147483647

## Returns the lowest number that can be stored in a [U32] without underflowing
## its available memory and crashing.
##
## For reference, this number is zero, because [U32] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
min_u32 : U32
min_u32 = 0

## Returns the highest number that can be stored in a [U32] without overflowing
## its available memory and crashing.
##
## For reference, this number is `4_294_967_295`.
max_u32 : U32
max_u32 = 4294967295

## Returns the lowest number that can be stored in an [I64] without underflowing
## its available memory and crashing.
##
## For reference, this number is `-9_223_372_036_854_775_808`,
## which is under 9 quintillion.
##
## Note that the positive version of this number is larger than [Num.max_i64],
## which means if you call [Num.abs] on [Num.min_i64], it will overflow and crash!
min_i64 : I64
min_i64 = -9223372036854775808

## Returns the highest number that can be stored in an [I64] without overflowing
## its available memory and crashing.
##
## For reference, this number is `9_223_372_036_854_775_807`,
## which is over 9 quintillion.
##
## Note that this is smaller than the positive version of [Num.min_i64],
## which means if you call [Num.abs] on [Num.min_i64], it will overflow and crash!
max_i64 : I64
max_i64 = 9223372036854775807

## Returns the lowest number that can be stored in a [U64] without underflowing
## its available memory and crashing.
##
## For reference, this number is zero, because [U64] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
min_u64 : U64
min_u64 = 0

## Returns the highest number that can be stored in a [U64] without overflowing
## its available memory and crashing.
##
## For reference, this number is `18_446_744_073_709_551_615`,
## which is over 18 quintillion.
max_u64 : U64
max_u64 = 18446744073709551615

## Returns the lowest number that can be stored in an [I128] without underflowing
## its available memory and crashing.
##
## For reference, this number is `-170_141_183_460_469_231_731_687_303_715_884_105_728`.
## which is under 170 undecillion.
##
## Note that the positive version of this number is larger than [Num.max_i128],
## which means if you call [Num.abs] on [Num.min_i128], it will overflow and crash!
min_i128 : I128
min_i128 = -170141183460469231731687303715884105728

## Returns the highest number that can be stored in an [I128] without overflowing
## its available memory and crashing.
##
## For reference, this number is `170_141_183_460_469_231_731_687_303_715_884_105_727`,
## which is over 170 undecillion.
##
## Note that this is smaller than the positive version of [Num.min_i128],
## which means if you call [Num.abs] on [Num.min_i128], it will overflow and crash!
max_i128 : I128
max_i128 = 170141183460469231731687303715884105727

## Returns the lowest number that can be stored in a [U128] without underflowing
## its available memory and crashing.
##
## For reference, this number is zero, because [U128] is
## [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number.
## Unsigned numbers cannot be negative.
min_u128 : U128
min_u128 = 0

## Returns the highest number that can be stored in a [U128] without overflowing
## its available memory and crashing.
##
## For reference, this number is `340_282_366_920_938_463_463_374_607_431_768_211_455`,
## which is over 340 undecillion.
max_u128 : U128
max_u128 = 340282366920938463463374607431768211455

min_f32 : F32
min_f32 = -3.40282347e38

max_f32 : F32
max_f32 = 3.40282347e38

min_f64 : F64
min_f64 = -1.7976931348623157e308

max_f64 : F64
max_f64 = 1.7976931348623157e308

## Converts an [Int] to an [I8]. If the given number can't be precisely represented in an [I8],
## the returned number may be different from the given number.
to_i8 : Int * -> I8
to_i16 : Int * -> I16
to_i32 : Int * -> I32
to_i64 : Int * -> I64
to_i128 : Int * -> I128
to_u8 : Int * -> U8
to_u16 : Int * -> U16
to_u32 : Int * -> U32
to_u64 : Int * -> U64
to_u128 : Int * -> U128

## Converts a [Num] to an [F32]. If the given number can't be precisely represented in an [F32],
## the returned number may be different from the given number.
to_f32 : Num * -> F32

## Converts a [Num] to an [F64]. If the given number can't be precisely represented in an [F64],
## the returned number may be different from the given number.
to_f64 : Num * -> F64

## Converts a [Int] to an [I8].
## If the given integer can't be precisely represented in an [I8], returns
## `Err OutOfBounds`.
to_i8_checked : Int * -> Result I8 [OutOfBounds]
to_i16_checked : Int * -> Result I16 [OutOfBounds]
to_i32_checked : Int * -> Result I32 [OutOfBounds]
to_i64_checked : Int * -> Result I64 [OutOfBounds]
to_i128_checked : Int * -> Result I128 [OutOfBounds]
to_u8_checked : Int * -> Result U8 [OutOfBounds]
to_u16_checked : Int * -> Result U16 [OutOfBounds]
to_u32_checked : Int * -> Result U32 [OutOfBounds]
to_u64_checked : Int * -> Result U64 [OutOfBounds]
to_u128_checked : Int * -> Result U128 [OutOfBounds]
to_f32_checked : Num * -> Result F32 [OutOfBounds]
to_f64_checked : Num * -> Result F64 [OutOfBounds]

## Turns a [Dec] into its [I128] representation by removing the decimal point.
## This is equivalent to multiplying the [Dec] by 10^18.
without_decimal_point : Dec -> I128

## Turns a [I128] into the coresponding [Dec] by adding the decimal point.
## This is equivalent to dividing the [I128] by 10^18.
with_decimal_point : I128 -> Dec

## Splits a [F32] into its components according to IEEE 754 standard.
f32_to_parts : F32 -> { sign : Bool, exponent : U8, fraction : U32 }

## Splits a [F64] into its components according to IEEE 754 standard.
f64_to_parts : F64 -> { sign : Bool, exponent : U16, fraction : U64 }

## Combine parts of a [F32] according to IEEE 754 standard.
## The fraction should not be bigger than 0x007F_FFFF, any bigger value will be truncated.
f32_from_parts : { sign : Bool, exponent : U8, fraction : U32 } -> F32

## Combine parts of a [F64] according to IEEE 754 standard.
## The fraction should not be bigger than 0x000F_FFFF_FFFF_FFFF, any bigger value will be truncated.
## The exponent should not be bigger than 0x07FF, any bigger value will be truncated.
f64_from_parts : { sign : Bool, exponent : U16, fraction : U64 } -> F64

## Convert a `Bool` to a `Num`
## ```roc
## expect Num.from_bool(Bool.true) == 1
## expect Num.from_bool(Bool.false) == 0
## ```
from_bool : Bool -> Num *
from_bool = |bool|
    if bool then
        1
    else
        0

## The value for not-a-number for a [F32] according to the IEEE 754 standard.
nan_f32 : F32
nan_f32 = 0.0f32 / 0.0

## The value for not-a-number for a [F64] according to the IEEE 754 standard.
nan_f64 : F64
nan_f64 = 0.0f64 / 0.0

## The value for infinity for a [F32] according to the IEEE 754 standard.
infinity_f32 : F32
infinity_f32 = 1.0f32 / 0.0

## The value for infinity for a [F64] according to the IEEE 754 standard.
infinity_f64 : F64
infinity_f64 = 1.0f64 / 0.0

