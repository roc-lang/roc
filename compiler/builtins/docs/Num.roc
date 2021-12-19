interface Num
    exposes
        [
            Num,
            Binary64,
            Binary32,
            Dec,
            Decimal,
            Float,
            FloatingPoint,
            F32,
            F64,
            I8,
            I16,
            I32,
            I64,
            I128,
            Int,
            Integer,
            Nat,
            Natural,
            Signed8,
            Signed16,
            Signed32,
            Signed64,
            Signed128,
            U8,
            U16,
            U32,
            U64,
            U128,
            Unsigned8,
            Unsigned16,
            Unsigned32,
            Unsigned64,
            Unsigned128,
            abs,
            acos,
            add,
            addChecked,
            addWrap,
            atan,
            bitwiseAnd,
            bitwiseOr,
            bitwiseXor,
            ceiling,
            compare,
            cos,
            div,
            divFloor,
            floor,
            intCast,
            isEven,
            isGt,
            isGte,
            isLt,
            isLte,
            isMultipleOf,
            isNegative,
            isOdd,
            isPositive,
            isZero,
            log,
            maxFloat,
            maxI128,
            maxInt,
            minFloat,
            minInt,
            modInt,
            modFloat,
            mul,
            mulChecked,
            mulWrap,
            neg,
            pow,
            powInt,
            rem,
            round,
            shiftLeftBy,
            shiftRightBy,
            shiftRightZfBy,
            sin,
            sub,
            subChecked,
            subWrap,
            sqrt,
            tan,
            toFloat,
            toStr
        ]
    imports []

## ## Types

## Represents a number that could be either an [Int] or a [Float].
##
## This is useful for functions that can work on either, for example #Num.add, whose type is:
##
## ```
## add : Num a, Num a -> Num a
## ```
##
## The number 1.5 technically has the type `Num (Fraction *)`, so when you pass
## two of them to [Num.add], the answer you get is `3.0 : Num (Fraction *)`.
#
## Similarly, the number 0x1 (that is, the integer 1 in hexadecimal notation)
## technically has the type `Num (Integer *)`, so when you pass two of them to
## [Num.add], the answer you get is `2 : Num (Integer *)`.
##
## The type [`Float a`]([Float]) is defined to be an alias for `Num (Fraction a)`,
## so `3.0 : Num (Fraction *)` is the same value as `3.0 : Float *`.
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
Num a : [ @Num a ]

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
Dec : Float [ @Decimal128 ]

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
## The value of `wasItPrecise` here will be `True`, because Roc uses [Dec]
## by default when there are no types specified.
##
## In contrast, suppose we use `f32` or `f64` for one of these numbers:
##
##     wasItPrecise = 0.1f64 + 0.2 == 0.3
##
## Here, `wasItPrecise` will be `False` because the entire calculation will have
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
## you ended up with one of them by using [isNaN], [isFinite], and [isInfinite].
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
Float a : Num [ @Fraction a ]

## A fixed-size integer - that is, a number with no fractional component.
##
## Integers come in two flavors: signed and unsigned. Signed integers can be
## negative ("signed" refers to how they can incorporate a minus sign),
## whereas unsigned integers cannot be negative.
##
## Since integers have a fixed size, the size you choose determines both the
## range of numbers it can represent, and also how much memory it takes up.
##
## #U8 is an an example of an integer. It is an unsigned #Int that takes up 8 bits
## (aka 1 byte) in memory. The `U` is for Unsigned and the 8 is for 8 bits.
## Because it has 8 bits to work with, it can store 256 numbers (2^8),
## and because it is unsigned, its min value is 0. This means the 256 numbers
## it can store range from 0 to 255.
##
## #I8 is a signed integer that takes up 8 bits. The `I` is for Integer, since
## integers in mathematics are signed by default. Because it has 8 bits just
## like #U8, it can store 256 numbers (still 2^16), but because it is signed,
## the range is different. Its 256 numbers range from -128 to 127.
##
## Here are some other examples:
##
## * #U16 is like #U8, except it takes up 16 bytes in memory. It can store 65,536 numbers (2^16), ranging from 0 to 65,536.
## * #I16 is like #U16, except it is signed. It can still store the same 65,536 numbers (2^16), ranging from -32,768 to 32,767.
##
## This pattern continues up to #U128 and #I128.
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
## CPU is typically fastest at executing instructions on #U64 and #I64 values,
## whereas a 32-bit CPU is typically fastest on #U32 and #I32 values.
##
## Putting these factors together, here are some reasonable guidelines for optimizing performance through integer size choice:
##
## * Start by deciding if this integer should allow negative numbers, and choose signed or unsigned accordingly.
## * Next, think about the range of numbers you expect this number to hold. Choose the smallest size you will never expect to overflow, no matter the inputs your program receives. (Validating inputs for size, and presenting the user with an error if they are too big, can help guard against overflow.)
## * Finally, if a particular numeric calculation is running too slowly, you can try experimenting with other number sizes. This rarely makes a meaningful difference, but some processors can operate on different number sizes at different speeds.
Int size : Num [ @Integer size ]

## A signed 8-bit integer, ranging from -128 to 127
I8 : Int [ @Signed8 ]
U8 : Int [ @Unsigned8 ]
I16 : Int [ @Signed16 ]
U16 : Int [ @Unsigned16 ]
I32 : Int [ @Signed32 ]
U32 : Int [ @Unsigned32 ]
I64 : Int [ @Signed64 ]
U64 : Int [ @Unsigned64 ]
I128 : Int [ @Signed128 ]
U128 : Int [ @Unsigned128 ]

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
Nat : Int [ @Natural ]

## A 64-bit signed integer. All number literals without decimal points are compatible with #Int values.
##
## >>> 1
##
## >>> 0
##
## You can optionally put underscores in your #Int literals.
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
## | `                                                -128` | #I8   | 1 Byte   |
## | `                                                 127` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                                   0` | #U8   | 1 Byte   |
## | `                                                 255` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                             -32_768` | #I16  | 2 Bytes  |
## | `                                              32_767` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                                   0` | #U16  | 2 Bytes  |
## | `                                              65_535` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                      -2_147_483_648` | #I32  | 4 Bytes  |
## | `                                       2_147_483_647` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                                   0` | #U32  | 4 Bytes  |
## | ` (over 4 billion)                      4_294_967_295` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                          -9_223_372_036_854_775_808` | #I64  | 8 Bytes  |
## | `                           9_223_372_036_854_775_807` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `                                                   0` | #U64  | 8 Bytes  |
## | ` (over 18 quintillion)    18_446_744_073_709_551_615` |       |          |
## |--------------------------------------------------------|-------|----------|
## | `-170_141_183_460_469_231_731_687_303_715_884_105_728` | #I128 | 16 Bytes |
## | ` 170_141_183_460_469_231_731_687_303_715_884_105_727` |       |          |
## |--------------------------------------------------------|-------|----------|
## | ` (over 340 undecillion)                            0` | #U128 | 16 Bytes |
## | ` 340_282_366_920_938_463_463_374_607_431_768_211_455` |       |          |
##
## Roc also has one variable-size integer type: #Nat. The size of #Nat is equal
## to the size of a memory address, which varies by system. For example, when
## compiling for a 64-bit system, #Nat is the same as #U64. When compiling for a
## 32-bit system, it's the same as #U32.
##
## A common use for #Nat is to store the length ("len" for short) of a
## collection like #List, #Set, or #Map. 64-bit systems can represent longer
## lists in memory than 32-bit systems can, which is why the length of a list
## is represented as a #Nat in Roc.
##
## If any operation would result in an #Int that is either too big
## or too small to fit in that range (e.g. calling `Int.maxI32 + 1`),
## then the operation will *overflow*. When an overflow occurs, the program will crash.
##
## As such, it's very important to design your code not to exceed these bounds!
## If you need to do math outside these bounds, consider using a larger numeric size.
Int size : Num [ @Int size ]

## Convert

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
## This is safe to use with any [Float], but it can cause overflow when used with certain #Int values.
##
## For example, calling #Num.neg on the lowest value of a signed integer (such as #Int.lowestI64 or #Int.lowestI32) will cause overflow.
## This is because, for any given size of signed integer (32-bit, 64-bit, etc.) its negated lowest value turns out to be 1 higher than
## the highest value it can represent. (For this reason, calling #Num.abs on the lowest signed value will also cause overflow.)
##
## Additionally, calling #Num.neg on any unsigned integer (such as any #U64 or #U32 value) other than zero will cause overflow.
##
## (It will never crash when given a [Float], however, because of how floating point numbers represent positive and negative numbers.)
neg : Num a -> Num a

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
## This is safe to use with any [Float], but it can cause overflow when used with certain #Int values.
##
## For example, calling #Num.abs on the lowest value of a signed integer (such as #Int.lowestI64 or #Int.lowestI32) will cause overflow.
## This is because, for any given size of signed integer (32-bit, 64-bit, etc.) its negated lowest value turns out to be 1 higher than
## the highest value it can represent. (For this reason, calling #Num.neg on the lowest signed value will also cause overflow.)
##
## Calling this on an unsigned integer (like #U32 or #U64) never does anything.
abs : Num a -> Num a

## Check

## The same as using `== 0` on the number.
isZero : Num * -> Bool

## Positive numbers are greater than 0.
isPositive : Num * -> Bool

## Negative numbers are less than 0.
isNegative : Num * -> Bool

## A number is even if dividing it by 2 gives a remainder of 0.
##
## Examples of even numbers: 0, 2, 4, 6, 8, -2, -4, -6, -8
isEven : Num * -> Bool

## A number is odd if dividing it by 2 gives a remainder of 1.
##
## Examples of odd numbers: 1, 3, 5, 7, -1, -3, -5, -7
isOdd : Num * -> Bool

## Arithmetic

## Add two numbers of the same type.
##
## (To add an #Int and a [Float], first convert one so that they both have the same type. There are functions in the [`Frac`](/Frac) module that can convert both #Int to [Float] and the other way around.)
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

## Add two numbers and check for overflow.
##
## This is the same as [Num.add] except if the operation overflows, instead of
## panicking or returning ∞ or -∞, it will return `Err Overflow`.
addCheckOverflow : Num a, Num a -> Result (Num a) [ Overflow ]*

## Subtract two numbers of the same type.
##
## (To subtract an #Int and a [Float], first convert one so that they both have the same type. There are functions in the [`Frac`](/Frac) module that can convert both #Int to [Float] and the other way around.)
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

## Subtract two numbers and check for overflow.
##
## This is the same as [Num.sub] except if the operation overflows, instead of
## panicking or returning ∞ or -∞, it will return `Err Overflow`.
subCheckOverflow : Num a, Num a -> Result (Num a) [ Overflow ]*

## Multiply two numbers of the same type.
##
## (To multiply an #Int and a [Float], first convert one so that they both have the same type. There are functions in the [`Frac`](/Frac) module that can convert both #Int to [Float] and the other way around.)
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

## Multiply two numbers and check for overflow.
##
## This is the same as [Num.mul] except if the operation overflows, instead of
## panicking or returning ∞ or -∞, it will return `Err Overflow`.
mulCheckOverflow : Num a, Num a -> Result (Num a) [ Overflow ]*

## Convert

## Convert a number to a [Str].
##
## This is the same as calling `Num.format {}` - so for more details on
## exact formatting, see [Num.format].
##
## >>> Num.toStr 42
##
## Only [Float] values will include a decimal point, and they will always include one.
##
## >>> Num.toStr 4.2
##
## >>> Num.toStr 4.0
##
## When this function is given a non-[finite](Num.isFinite)
## [F64] or [F32] value, the returned string will be `"NaN"`, `"∞"`, or `"-∞"`.
##
## To get strings in hexadecimal, octal, or binary format, use [Num.format].
toStr : Num * -> Str

## Convert a number into a [Str], formatted with the given options.
##
## Default options:
## * `base: Decimal`
## * `notation: Standard`
## * `decimalMark: HideForIntegers "."`
## * `decimalDigits: { min: 0, max: All }`
## * `minIntDigits: 1`
## * `wholeSep: { mark: ",", places: 3 }`
##
## ## Options
##
##
## ### decimalMark
##
## * `AlwaysShow` always shows the decimal mark, no matter what.
## * `HideForIntegers` hides the decimal mark if all the numbers after the decimal mark are 0.
##
## The [Str] included in either of these represents the mark itself.
##
## ### `decimalDigits
##
## With 0 decimal digits, the decimal mark will still be rendered if
## `decimalMark` is set to `AlwaysShow`.
##
## If `max` is less than `min`, then first the number will be truncated to `max`
## digits, and then zeroes will be added afterwards until it reaches `min` digits.
##
## >>> Num.format 1.23 { decPlaces: 0, decPointVis: AlwaysShow }
##
## ### minIntDigits
##
## If the integer portion of number is fewer than this many digits, zeroes will
## be added in front of it until there are at least `minWholeDigits` digits.
##
## If this is set to zero, then numbers less than 1 will begin with `"."`
## rather than `"0."`.
##
## ### wholeSep
##
## Examples:
##
## In some countries (e.g. USA and UK), a comma is used to separate thousands:
## >>> Num.format 1_000_000 { pf: Decimal, wholeSep: { mark: ",", places: 3 } }
##
## Sometimes when rendering bits, it's nice to group them into groups of 4:
## >>> Num.format 1_000_000 { pf: Binary, wholeSep: { mark: " ", places: 4 } }
##
## It's also common to render hexadecimal in groups of 2:
## >>> Num.format 1_000_000 { pf: Hexadecimal, wholeSep: { mark: " ", places: 2 } }
format :
    Num *,
    {
        base ? [ Decimal, Hexadecimal, Octal, Binary ],
        notation ? [ Standard, Scientific ],
        decimalMark ? [ AlwaysShow Str, HideForIntegers ],
        decimalDigits ? { min : U16, max : [ All, Trunc U16, Round U16, Floor U16, Ceil U16 ] },
        minWholeDigits ? U16,
        wholeSep ? { mark : Str, places : U64 }
    }
    -> Str

## Round off the given float to the nearest integer.
round : Float * -> Int *
ceil : Float * -> Int *
floor : Float * -> Int *
trunc : Float * -> Int *

## Convert an #Int to a #Nat. If the given number doesn't fit in #Nat, it will be truncated.
## Since #Nat has a different maximum number depending on the system you're building
## for, this may give a different answer on different systems.
##
## For example, on a 32-bit system, [Num.maxNat] will return the same answer as
## #Num.maxU32. This means that calling `Num.toNat 9_000_000_000` on a 32-bit
## system will return #Num.maxU32 instead of 9 billion, because 9 billion is
## higher than #Num.maxU32 and will not fit in a #Nat on a 32-bit system.
##
## However, calling `Num.toNat 9_000_000_000` on a 64-bit system will return
## the #Nat value of 9_000_000_000. This is because on a 64-bit system, #Nat can
## hold up to #Num.maxU64, and 9_000_000_000 is lower than #Num.maxU64.
##
## To convert a [Float] to a #Nat, first call either #Num.round, #Num.ceil, or #Num.floor
## on it, then call this on the resulting #Int.
toNat : Int * -> Nat

## Convert an #Int to an #I8. If the given number doesn't fit in #I8, it will be truncated.
##
## To convert a [Float] to an #I8, first call either #Num.round, #Num.ceil, or #Num.floor
## on it, then call this on the resulting #Int.
toI8 : Int * -> I8
toI16 : Int * -> I16
toI32 : Int * -> I32
toI64 : Int * -> I64
toI128 : Int * -> I128

## Convert an #Int to an #U8. If the given number doesn't fit in #U8, it will be truncated.
## Crashes if the given number is negative.
toU8 : Int * -> U8
toU16 : Int * -> U16
toU32 : Int * -> U32
toU64 : Int * -> U64
toU128 : Int * -> U128

## Convert a #Num to a #F32. If the given number can't be precisely represented in a #F32,
## there will be a loss of precision.
toF32 : Num * -> F32

## Convert a #Num to a #F64. If the given number can't be precisely represented in a #F64,
## there will be a loss of precision.
toF64 : Num * -> F64

## Convert a #Num to a #Dec. If the given number can't be precisely represented in a #Dec,
## there will be a loss of precision.
toDec : Num * -> Dec

## Divide two integers and #Num.round  the resulut.
##
## Division by zero is undefined in mathematics. As such, you should make
## sure never to pass zero as the denomaintor to this function!
##
## If zero does get passed as the denominator...
##
## * In a development build, you'll get an assertion failure.
## * In an optimized build, the function will return 0.
##
## `a // b` is shorthand for `Num.divRound a b`.
##
## >>> 5 // 7
##
## >>> Num.divRound 5 7
##
## >>> 8 // -3
##
## >>> Num.divRound 8 -3
##
## This is the same as the #// operator.
divRound : Int a, Int a -> Int a

## Perform flooring modulo on two integers.
##
## Modulo is the same as remainder when working with positive numbers,
## but if either number is negative, then modulo works differently.
##
## Additionally, flooring modulo uses [Float].floor on the result.
##
## (Use [Float].mod for non-flooring modulo.)
##
## Return `Err DivByZero` if the second integer is zero, because division by zero is undefined in mathematics.
##
## `a %% b` is shorthand for `Int.modFloor a b`.
##
## >>> 5 %% 7
##
## >>> Int.modFloor 5 7
##
## >>> -8 %% -3
##
## >>> Int.modFloor -8 -3
#modFloor : Int a, Int a -> Result (Int a) [ DivByZero ]*


## Bitwise

xor : Int a, Int a -> Int a

and : Int a, Int a -> Int a

not : Int a -> Int a

## Limits

## The highest number that can be stored in a #Nat without overflowing its
## available memory and crashing.
##
## Note that this number varies by systems. For example, when building for a
## 64-bit system, this will be equal to #Num.maxU64, but when building for a
## 32-bit system, this will be equal to #Num.maxU32.
maxNat : Nat

## The number zero.
##
## #Num.minNat is the lowest number that can be stored in a #Nat, which is zero
## because #Nat is [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number. Unsigned numbers cannot be negative.
minNat : Nat

## The highest number that can be stored in an #I32 without overflowing its
## available memory and crashing.
##
## Note that this is smaller than the positive version of #Int.minI32
## which means if you call #Num.abs on #Int.minI32, it will overflow and crash!
maxI32 : I32

## The min number that can be stored in an #I32 without overflowing its
## available memory and crashing.
##
## Note that the positive version of this number is this is larger than
## #Int.maxI32, which means if you call #Num.abs on #Int.minI32, it will overflow and crash!
minI32 : I32

## The highest number that can be stored in a #U64 without overflowing its
## available memory and crashing.
##
## For reference, that number is `18_446_744_073_709_551_615`, which is over 18 quintillion.
maxU64 : U64

## The number zero.
##
## #Num.minU64 is the lowest number that can be stored in a #U64, which is zero
## because #U64 is [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number. Unsigned numbers cannot be negative.
minU64 : U64

## The highest number that can be stored in a #U32 without overflowing its
## available memory and crashing.
##
## For reference, that number is `4_294_967_295`, which is over 4 million.
maxU32 : U32

## The number zero.
##
## #Num.minU32 is the lowest number that can be stored in a #U32, which is zero
## because #U32 is [unsigned](https://en.wikipedia.org/wiki/Signed_number_representations),
## and zero is the lowest unsigned number. Unsigned numbers cannot be negative.
minU32 : U32

## The highest supported #F64 value you can have, which is approximately 1.8 × 10^308.
##
## If you go higher than this, your running Roc code will crash - so be careful not to!
maxF64 : F64

## The lowest supported #F64 value you can have, which is approximately -1.8 × 10^308.
##
## If you go lower than this, your running Roc code will crash - so be careful not to!
minF64 : F64

## The highest supported #F32 value you can have, which is approximately 1.8 × 10^308.
##
## If you go higher than this, your running Roc code will crash - so be careful not to!
maxF32 : F32

## The lowest supported #F32 value you can have, which is approximately -1.8 × 10^308.
##
## If you go lower than this, your running Roc code will crash - so be careful not to!
minF32 : F32

## The highest supported #Dec value you can have, which is precisely 170_141_183_460_469_231_731.687303715884105727.
##
## If you go higher than this, your running Roc code will crash - so be careful not to!
maxDec : Dec

## The lowest supported #Dec value you can have, which is precisely -170_141_183_460_469_231_731.687303715884105728.
##
## If you go lower than this, your running Roc code will crash - so be careful not to!
minDec : Dec

## Constants

## An approximation of e, specifically 2.718281828459045.
e : Float *

## An approximation of pi, specifically 3.141592653589793.
pi : Float *

## Trigonometry

cos : Float a -> Float a

acos : Float a -> Float a

sin : Float a -> Float a

asin : Float a -> Float a

tan : Float a -> Float a

atan : Float a -> Float a

## Other Calculations (arithmetic?)

## Divide one [Float] by another.
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
## To divide an [Int] and a [Float], first convert the [Int] to a [Float] using
## one of the functions in this module like [toDec].
##
## >>> 5.0 / 7.0
##
## >>> Num.div 5 7
##
## `Num.div` can be convenient in pipelines.
##
## >>> Num.pi
## >>>     |> Num.div 2.0
div : Float a, Float a -> Float a

## Perform modulo on two [Float]s.
##
## Modulo is the same as remainder when working with positive numbers,
## but if either number is negative, then modulo works differently.
##
## `a % b` is shorthand for `Num.mod a b`.
##
## [Division by zero is undefined in mathematics](https://en.wikipedia.org/wiki/Division_by_zero),
## and as such, so is modulo by zero. Because of this, you should make sure never
## to pass zero for the second argument to this function!
##
## Passing [mod] a [Dec] value of zero for its second argument will cause a panic.
## Passing [mod] a [F32] and [F64] value for its second argument will cause it
## to return [*NaN*](Num.isNaN).
##
## >>> 5.0 % 7.0
##
## >>> Num.mod 5 7
##
## `Num.mod` can be convenient in pipelines.
##
## >>> Num.pi
## >>>     |> Num.mod 2.0
mod : Float a, Float a -> Float a

## Raises a [Float] to the power of another [Float].
##
## `
## For an #Int alternative to this function, see #Num.raise.
pow : Float a, Float a -> Float a

## Raises an integer to the power of another, by multiplying the integer by
## itself the given number of times.
##
## This process is known as [exponentiation by squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring).
##
## For a [Float] alternative to this function, which supports negative exponents,
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
## Be careful! Even though this function takes only a #U8, it is very easy to
## overflow
expBySquaring : Int a, U8 -> Int a

## Returns an approximation of the absolute value of a [Float]'s square root.
##
## The square root of a negative number is an irrational number, and [Float] only
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
## >>> Frac.sqrt 4.0
##
## >>> Frac.sqrt 1.5
##
## >>> Frac.sqrt 0.0
##
## >>> Frac.sqrt -4.0f64
##
## >>> Frac.sqrt -4.0dec
sqrt : Float a -> Float a

## Bit shifts

## [Logical bit shift](https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift) left.
##
## `a << b` is shorthand for `Num.shl a b`.
shl : Int a, Int a -> Int a

## [Arithmetic bit shift](https://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift) left.
##
## This is called `shlWrap` because any bits shifted
## off the beginning of the number will be wrapped around to
## the end. (In contrast, [shl] replaces discarded bits with zeroes.)
shlWrap : Int a, Int a -> Int a

## [Logical bit shift](https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift) right.
##
## `a >> b` is shorthand for `Num.shr a b`.
shr : Int a, Int a -> Int a

## [Arithmetic bit shift](https://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift) right.
##
## This is called `shlWrap` because any bits shifted
## off the end of the number will be wrapped around to
## the beginning. (In contrast, [shr] replaces discarded bits with zeroes.)
shrWrap : Int a, Int a -> Int a

## [Endianness](https://en.wikipedia.org/wiki/Endianness)
# Endi : [ Big, Little, Native ]

## The `Endi` argument does not matter for [U8] and [I8], since they have
## only one byte.
# toBytes : Num *, Endi -> List U8

## when Num.parseBytes bytes Big is
##     Ok { val: f64, rest } -> ...
##     Err (ExpectedNum (Float Binary64)) -> ...
# parseBytes : List U8, Endi -> Result { val : Num a, rest : List U8 } [ ExpectedNum a ]*

## when Num.fromBytes bytes Big is
##     Ok f64 -> ...
##     Err (ExpectedNum (Float Binary64)) -> ...
# fromBytes : List U8, Endi -> Result (Num a) [ ExpectedNum a ]*

## Comparison

## Returns `True` if the first number is less than the second.
##
## `a < b` is shorthand for `Num.isLt a b`.
##
## If either argument is [*NaN*](Num.isNaN), returns `False` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
##
## >>> 5
## >>>     |> Num.isLt 6
isLt : Num a, Num a -> Bool

## Returns `True` if the first number is less than or equal to the second.
##
## `a <= b` is shorthand for `Num.isLte a b`.
##
## If either argument is [*NaN*](Num.isNaN), returns `False` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
isLte : Num a, Num a -> Bool

## Returns `True` if the first number is greater than the second.
##
## `a > b` is shorthand for `Num.isGt a b`.
##
## If either argument is [*NaN*](Num.isNaN), returns `False` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
##
## >>> 6
## >>>     |> Num.isGt 5
isGt : Num a, Num a -> Bool

## Returns `True` if the first number is greater than or equal to the second.
##
## `a >= b` is shorthand for `Num.isGte a b`.
##
## If either argument is [*NaN*](Num.isNaN), returns `False` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
isGte : Num a, Num a -> Bool

## Returns the higher of two numbers.
##
## If either argument is [*NaN*](Num.isNaN), returns `False` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
higher : Num a, Num a -> Num a

## Returns the lower of two numbers.
##
## If either argument is [*NaN*](Num.isNaN), returns `False` no matter what. (*NaN*
## is [defined to be unordered](https://en.wikipedia.org/wiki/NaN#Comparison_with_NaN).)
lower : Num a, Num a -> Num a

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
compare : Num a, Num a -> [ Lt, Eq, Gt ]

## Special Floating-Point Values

## When given a [F64] or [F32] value, returns `False` if that value is
## [*NaN*](Num.isNaN), ∞ or -∞, and `True` otherwise.
##
## Always returns `True` when given a [Dec].
##
## This is the opposite of [isInfinite], except when given [*NaN*](Num.isNaN). Both
## [isFinite] and [isInfinite] return `False` for [*NaN*](Num.isNaN).
isFinite : Float * -> Bool

## When given a [F64] or [F32] value, returns `True` if that value is either
## ∞ or -∞, and `False` otherwise.
##
## Always returns `False` when given a [Dec].
##
## This is the opposite of [isFinite], except when given [*NaN*](Num.isNaN). Both
## [isFinite] and [isInfinite] return `False` for [*NaN*](Num.isNaN).
isInfinite : Float * -> Bool

## When given a [F64] or [F32] value, returns `True` if that value is
## *NaN* ([not a number](https://en.wikipedia.org/wiki/NaN)), and `False` otherwise.
##
## Always returns `False` when given a [Dec].
##
## >>> Num.isNaN 12.3
##
## >>> Num.isNaN (Num.sqrt -2)
##
## *NaN* is unusual from other numberic values in that:
## * *NaN* is not equal to any other number, even itself. [Bool.isEq] always returns `False` if either argument is *NaN*.
## * *NaN* has no ordering, so [isLt], [isLte], [isGt], and [isGte] always return `False` if either argument is *NaN*.
##
## These rules come from the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
## floating point standard. Because almost all modern processors are built to
## this standard, deviating from these rules has a significant performance
## cost! Since the most common reason to choose [F64] or [F32] over [Dec] is
## access to hardware-accelerated performance, Roc follows these rules exactly.
##
## Note that you should never put a *NaN* into a [Set], or use it as the key in
## a [Dict]. The result is entries that can never be removed from those
## collections! See the documentation for [Set.add] and [Dict.insert] for details.
isNaN : Float * -> Bool
