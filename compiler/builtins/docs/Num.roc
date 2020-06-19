interface Num
    exposes [ Num, neg, abs, add, sub, mul, isOdd, isEven, isPositive, isNegative, isZero ]
    imports []

## Types

## Represents a number that could be either an #Int or a #Float.
##
## This is useful for functions that can work on either, for example #Num.add, whose type is:
##
## ```
## add : Num range, Num range -> Num range
## ```
##
## The number 1.5 technically has the type `Num FloatingPoint`, so when you pass two of them to `Num.add`, the answer you get is `3.0 : Num FloatingPoint`.
##
## The type #Float is defined to be an alias for `Num FloatingPoint`, so `3.0 : Num FloatingPoint` is the same answer as `3.0 : Float`.  # # Similarly, the number 1 technically has the type `Num Integer`, so when you pass two of them to `Num.add`, the answer you get is `2 : Num Integer`.  # # The type #Int is defined to be an alias for `Num Integer`, so `2 : Num Integer` is the same answer as `2 : Int`.  #
## In this way, the `Num` type makes it possible to have `1 + 1` return `2 : Int` and `1.5 + 1.5` return `3.0 : Float`.
##
## ## Number Literals
##
## Number literals without decimal points (like `0`, `4` or `360`)
## have the type `Num *` at first, but usually end up taking on
## a more specific type based on how they're used.
##
## For example, in `(1 + List.len myList)`, the `1` has the type `Num *` at first,
## but because `List.len` returns a `Ulen`, the `1` ends up changing from
## `Num *` to the more specific `Ulen`, and the expression as a whole
## ends up having the type `Ulen`.
##
## Sometimes number literals don't become more specific. For example,
## the #Num.toStr function has the type `Num * -> Str`. This means that
## when calling `Num.toStr (5 + 6)`, the expression `(5 + 6)`
## still has the type `Num *`. When this happens, `Num *` defaults to
## being an #I32 - so this addition expression would overflow
## if either 5 or 6 were replaced with a number big enough to cause
## addition overflow on an #I32.
##
## If this default of #I32 is not big enough for your purposes,
## you can add an `i64` to the end of the number literal, like so:
##
## >>> Num.toStr 5_000_000_000i64
##
## This `i64` suffix specifies that you want this number literal to be
## an #I64 instead of a `Num *`. All the other numeric types have
## suffixes just like `i64`; here are some other examples:
##
## * `215u8` is a `215` value of type #U8
## * `76.4f32` is a `76.4` value of type #F32
## * `12345ulen` is a `12345` value of type `#Ulen`
##
## In practice, these are rarely needed. It's most common to write
## number literals without any suffix.
Num range : @Num range

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
Int size : Num (@Int size)

## A signed 8-bit integer, ranging from -128 to 127
I8 : Int @I8
U8 : Int @U8
U16 : Int @U16
I16 : Int @I16
U32 : Int @U32
I32 : Int @I32
I64 : Int @I64
U64 : Int @U64
I128 : Int @I128
U128 : Int @U128
Ilen : Int @Ilen
Ulen : Int @Ulen

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
## There are also two variable-size integer types: #Ulen and #Ilen. Their sizes
## are determined by the [machine word length](https://en.wikipedia.org/wiki/Word_(computer_architecture))
## of the system you're compiling for. (The "len" in their names is short for "length of a machine word.")
## For example, when compiling for a 64-bit target, #Ulen is the same as #U64,
## and #Ilen is the same as #I64. When compiling for a 32-bit target, #Ulen is the same as #U32,
## and #Ilen is the same as #I32. In practice, #Ulen sees much more use than #Ilen.
##
## If any operation would result in an #Int that is either too big
## or too small to fit in that range (e.g. calling `Int.maxI32 + 1`),
## then the operation will *overflow*. When an overflow occurs, the program will crash.
##
## As such, it's very important to design your code not to exceed these bounds!
## If you need to do math outside these bounds, consider using a larger numeric size.
Int size : Num [ @Int size ]

## A 64-bit floating-point number. All number literals with decimal points are #Float values.
##
## >>> 0.1
##
## >>> 1.0
##
## >>> 0.0
##
## If you like, you can put underscores in your #Float literals.
## They have no effect on the number's value, but can make things easier to read.
##
## >>> 1_000_000.000_000_001
##
## Roc supports two types of floating-point numbers:
##
## - *Decimal* floating-point numbers
## - *Binary* floating-point numbers
##
## Decimal floats are precise for decimal calculations. For example:
##
## >>> 0.1 + 0.2
##
## Operations on binary floats tend to run *much* faster than operations on
## decimal floats, because almost all processors have dedicated instructions
## for binary floats and not for decimal floats.
## However, binary floats are less precise for decimal calculations.
##
## For example, here is the same `0.1 + 0.2` calculation again, this time putting
## `f64` after the numbers to specify that they should be #F64 binary floats
## instead of the default of decimal floats.
##
## >>> 0.1f64 + 0.2f64
##
## If decimal precision is unimportant, binary floats give better performance.
## If decimal precision is important - for example, when representing money -
## decimal floats tend to be worth the performance cost.
##
## Usually, Roc's compiler can infer a more specific type than #Float for
## a particular float value, based on how it is used with other numbers. For example:
##
## >>> coordinates : { x : F32, y : F32 }
## >>> coordinates = { x: 1, y: 2.5 }
## >>>
## >>> coordinates.x + 1
##
## On the last line, the compiler infers that the `1` in `+ 1` is an #F32
## beacuse it's being added to `coordinates.x`, which was defined to be an #F32
## on the first line.
##
## Sometimes the compiler has no information about which specific type to pick.
## For example:
##
## >>> 0.1 + 0.2 == 0.3
##
## When this happens, the compiler defaults to choosing #D64 decimal floats.
## If you want something else, you can write (for example) `0.1f32 + 0.2 == 0.3`
## to compare them as #F32 values instead.
##
## Both decimal and binary #Float values conform to the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754#Interchange_formats)
## specification for floating point numbers. Conforming to this specification
## means Roc's binary floats have nearly universal hardware support, and its
## decimal floats have [some hardware support](http://speleotrove.com/decimal/)
## among the rare processors which support decimal float instructions at all.
##
## This specification covers several float formats. Here are the ones Roc supports:
##
## - #F32 (32-bit binary float)
## - #F64 (64-bit binary float)
## - #D32 (32-bit decimal float)
## - #D64 (64-bit decimal float) # TODO show a table like we do with ints, with the min/max ranges
##
## Like #Int, it's possible for #Float operations to overflow. Like with ints,
## you'll typically get a crash when this happens.
##
## * In a development build, you'll get an assertion failure.
## * In an optimized build, you'll get [`Infinity` or `-Infinity`](https://en.wikipedia.org/wiki/IEEE_754-1985#Positive_and_negative_infinity).
##
## Although some languages treat have first-class representations for
## `-Infinity`, `Infinity`, and the special `NaN` ("not a number")
## floating-point values described in the IEEE-754, Roc does not.
## Instead, Roc treats all of these as errors. If any Float operation
## in a development build encounters one of these values, it will
## result in an assertion failure.
##
## Stll, it's possible that these values may accidentally arise in
## release builds. If this happens, they will behave according to the usual
## IEEE-754 rules: any operation involving `NaN` will output `NaN`,
## any operation involving `Infinity` or `-Infinity` will output either
## `Infinity`, `-Infinity`, or `NaN`, and `NaN` is defined to be not
## equal to itself - meaning `(x == x)` returns `False` if `x` is `NaN`.
##
## These are very error-prone values, so if you see an assertion fail in
## developent because of one of them, take it seriously - and try to fix
## the code so that it can't come up in a release!
##
## ## Loud versus Quiet errors
##
## Besides precision problems, another reason floats are error-prone
## is that they have quiet error handling built in. For example, in
## a 64-bit floating point number, there are certain patterns of those
## 64 bits which do not represent valid floats; instead, they represent
## invalid results of previous operations.
##
## Whenever any arithmetic operation is performed on an invalid float,
## the result is also invalid. This is called *error propagation*, and
## it is notoriously error-prone. In Roc, using equality operations like
## `==` and `!=` on an invalid float causes a crash. (See #Float.verify
## to check the validity of your float.)
##
## Beause invalid floats are so error-prone, Roc discourages using them.
## Instead, by default it treats them the same way as overflow: by
## crashing whenever any #Float function would otherwise return one.
## You can also use functions like #Float.tryAdd to get an `Ok` or an error
## back so you can gracefully recover from invalid values.
##
## Quiet errors can be useful sometimes. For example, you might want to
## do three floating point calculations in a row, and then gracefully handle
## the situation where any one of the three was invalid. In that situation,
## quiet errors can be more efficient than using three `try` functions, because
## it can have one condition at the end instead of three along the way.
##
## Another potential use for quiet errors is for risky performance optimizations.
## When you are absolutely certain there is no chance of overflow or other
## errors, using a *quiet* operation may save an entry in the instruction cache
## by removing a branch that would always have been predicted correctly.
## Always [measure the performance change](https://youtu.be/r-TLSBdHe1A)
## if you do this! The compiler can optimize away those branches behind the scenes,
## so you may find that using the quiet version expliitly
## makes the code riskier to future change, without actually affecting performance.
##
## ## Performance Notes
##
## Currently, loud errors are implemented using an extra conditional. Although
## this conditional will always be correctly branh-predicted unless an error
## occurs, there is a small effect on the instruction cache, which means
## quiet errors are very slightly more efficient.
##
## Long-term, it's possible that the Roc compiler may be able to implement
## loud errors using *signalling errors* in some situations, which could
## eliminate the performance difference between loud and quiet errors in
## the situation where no error occurs.


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
## This is safe to use with any #Float, but it can cause overflow when used with certain #Int values.
##
## For example, calling #Num.neg on the lowest value of a signed integer (such as #Int.lowestI64 or #Int.lowestI32) will cause overflow.
## This is because, for any given size of signed integer (32-bit, 64-bit, etc.) its negated lowest value turns out to be 1 higher than
## the highest value it can represent. (For this reason, calling #Num.abs on the lowest signed value will also cause overflow.)
##
## Additionally, calling #Num.neg on any unsigned integer (such as any #U64 or #U32 value) other than 0 will cause overflow.
##
## (It will never crash when given a #Float, however, because of how floating point numbers represent positive and negative numbers.)
neg : Num range -> Num range

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
## This is safe to use with any #Float, but it can cause overflow when used with certain #Int values.
##
## For example, calling #Num.abs on the lowest value of a signed integer (such as #Int.lowestI64 or #Int.lowestI32) will cause overflow.
## This is because, for any given size of signed integer (32-bit, 64-bit, etc.) its negated lowest value turns out to be 1 higher than
## the highest value it can represent. (For this reason, calling #Num.neg on the lowest signed value will also cause overflow.)
##
## Calling this on an unsigned integer (like #U32 or #U64) never does anything.
abs : Num range -> Num range

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
## (To add an #Int and a #Float, first convert one so that they both have the same type. There are functions in the [`Float`](/Float) module that can convert both #Int to #Float and the other way around.)
##
## `a + b` is shorthand for `Num.add a b`.
##
## >>> 5 + 7
##
## >>> Num.add 5 7
##
## `Num.add` can be convenient in pipelines.
##
## >>> Float.pi
## >>>     |> Num.add 1.0
add : Num range, Num range -> Num range

## Subtract two numbers of the same type.
##
## (To subtract an #Int and a #Float, first convert one so that they both have the same type. There are functions in the [`Float`](/Float) module that can convert both #Int to #Float and the other way around.)
##
## `a - b` is shorthand for `Num.sub a b`.
##
## >>> 7 - 5
##
## >>> Num.sub 7 5
##
## `Num.sub` can be convenient in pipelines.
##
## >>> Float.pi
## >>>     |> Num.sub 2.0
sub : Num range, Num range -> Num range

## Multiply two numbers of the same type.
##
## (To multiply an #Int and a #Float, first convert one so that they both have the same type. There are functions in the [`Float`](/Float) module that can convert both #Int to #Float and the other way around.)
##
## `a * b` is shorthand for `Num.mul a b`.
##
## >>> 5 * 7
##
## >>> Num.mul 5 7
##
## `Num.mul` can be convenient in pipelines.
##
## >>> Float.pi
## >>>     |> Num.mul 2.0
mul : Num range, Num range -> Num range

## Convert

## Convert a number to a string, formatted as the traditional base 10 (decimal).
##
## >>> Num.toStr 42
##
## Only #Float values will include a decimal point, and they will always include one.
##
## >>> Num.toStr 4.2
##
## >>> Num.toStr 4.0
##
## For other bases see #toHexStr, #toOctalStr, and #toBinaryStr.
toStr : Num * -> Str

## Round off the given float to the nearest integer.
round : Float * -> Int *
ceil : Float * -> Int *
floor : Float * -> Int *
trunc : Float * -> Int *

## Convert an #Int to an #I8. If the given number doesn't fit in #I8, it will be truncated.
##
## To convert a #Float to an #I8, first call either #Num.round, #Num.ceil, or #Num.floor
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

## Convert a #Num to a #D32. If the given number can't be precisely represented in a #D32,
## there will be a loss of precision.
toD32 : Num * -> D32

## Convert a #Num to a #D64. If the given number can't be precisely represented in a #D64,
## there will be a loss of precision.
toD64 : Num * -> D64

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
divRound : Int, Int -> Int

## Perform flooring modulo on two integers.
##
## Modulo is the same as remainder when working with positive numbers,
## but if either number is negative, then modulo works differently.
##
## Additionally, flooring modulo uses #Float.floor on the result.
##
## (Use #Float.mod for non-flooring modulo.)
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
#modFloor : Int, Int -> Result DivByZero Int


## Bitwise

xor : Int -> Int -> Int

and : Int -> Int -> Int

not : Int -> Int

## Sort ascending - that is, with the lowest first, and the highest last.
##
##     List.sort Num.asc [ 3, 6, 0 ]
##
asc : Num a, Num a -> [ Eq, Lt, Gt ]

## Sort descending - that is, with the highest first, and the lowest last.
##
##     List.sort Num.desc [ 3, 6, 0 ]
##
desc : Num a, Num a -> [ Eq, Lt, Gt ]

## TODO should we offer hash32 etc even if someday it has to do a hash64 and truncate?
##
## This function can crash under these circumstances:
##
## * It receives a function, or any type that contains a function (for example a record, tag, or #List containing a function)
## * It receives an erroneous #Float (`NaN`, `Infinity`, or `-Infinity` - these values can only originate from hosts)
##
## CAUTION: This function may give different answers in future releases of Roc,
## so be aware that if you rely on the exact answer this gives today, your
## code may break in a future Roc release.
hash64 : a -> U64

## Limits

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

## The highest supported #Float value you can have, which is approximately 1.8 × 10^308.
##
## If you go higher than this, your running Roc code will crash - so be careful not to!
maxF64 : Float *

## The lowest supported #Float value you can have, which is approximately -1.8 × 10^308.
##
## If you go lower than this, your running Roc code will crash - so be careful not to!
minF64 : Float *

## The highest integer that can be represented as a #Float without # losing precision.
## It is equal to 2^53, which is approximately 9 × 10^15.
##
## Some integers higher than this can be represented, but they may lose precision. For example:
##
## >>> Float.highestInt
##
## >>> Float.highestInt + 100 # Increasing may lose precision
##
## >>> Float.highestInt - 100 # Decreasing is fine - but watch out for lowestLosslessInt!
maxPreciseInt : Float *

## The lowest integer that can be represented as a #Float without losing precision.
## It is equal to -2^53, which is approximately -9 × 10^15.
##
## Some integers lower than this can be represented, but they may lose precision. For example:
##
## >>> Float.lowestIntVal
##
## >>> Float.lowestIntVal - 100 # Decreasing may lose precision
##
## >>> Float.lowestIntVal + 100 # Increasing is fine - but watch out for highestInt!
maxPreciseInt : Float *

## Constants

## An approximation of e, specifically 2.718281828459045.
e : Float *

## An approximation of pi, specifically 3.141592653589793.
pi : Float *
## Constants

## An approximation of e, specifically 2.718281828459045.
e : Float *

## An approximation of pi, specifically 3.141592653589793.
pi : Float *

#ceiling : Float -> Int

#floor : Float -> Int

## Trigonometry

#cos : Float -> Float

#acos : Float -> Float

#sin : Float -> Float

#asin : Float -> Float

#tan : Float -> Float

#atan : Float -> Float

## Other Calculations (arithmetic?)

## Divide two #Float numbers.
##
## `a / b` is shorthand for `Float.div a b`.
##
## Division by zero is undefined in mathematics. As such, you should make
## sure never to pass zero as the denomaintor to this function!
##
## If zero does get passed as the denominator...
##
## * In a development build, you'll get an assertion failure.
## * In a release build, the function will return `Infinity`, `-Infinity`, or `NaN` depending on the arguments.
##
## To divide an #Int and a #Float, first convert the #Int to a #Float using one of the functions in this module.
##
## >>> 5.0 / 7.0
##
## >>> Float.div 5 7
##
## `Float.div` can be convenient in pipelines.
##
## >>> Float.pi
## >>>     |> Float.div 2.0
#div : Float, Float -> Result Float DivByZero
div = \numerator, denominator ->
    when numerator is
        0.0 -> 0.0 # TODO return Result!
        _ -> denominator

## Perform modulo on two #Float numbers.
##
## Modulo is the same as remainder when working with positive numbers,
## but if either number is negative, then modulo works differently.
##
## Return `Err DivByZero` if the second number is zero, because division by zero is undefined in mathematics.
##
## `a % b` is shorthand for `Float.mod a b`.
##
## >>> 5.0 % 7.0
##
## >>> Float.mod 5 7
##
## `Float.mod` can be convenient in pipelines.
##
## >>> Float.pi
## >>>     |> Float.mod 2.0
mod : Float a, Float a -> Result Float DivByZero

tryMod : Float a, Float a -> Result (Float a) [ DivByZero ]*

## Return the reciprocal of a #Float - that is, divides `1.0` by the given number.
##
## Crashes if given `0.0`, because division by zero is undefined in mathematics.
##
## For a version that does not crash, use #tryRecip
recip : Float a -> Result (Float a) [ DivByZero ]*


tryRecip : Float a -> Result (Float a) [ DivByZero ]*

## Return an approximation of the absolute value of the square root of the #Float.
##
## Return #InvalidSqrt if given a negative number or an invalid #Float. The square root of a negative number is an irrational number, and #Float only supports rational numbers.
##
## >>> Float.sqrt 4.0
##
## >>> Float.sqrt 1.5
##
## >>> Float.sqrt 0.0
##
## >>> Float.sqrt -4.0
sqrt : Float a -> [Ok (Float a), InvalidSqrt]*
