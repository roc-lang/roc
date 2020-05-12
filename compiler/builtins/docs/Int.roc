interface Int
    exposes [ Int ]
    imports []

## Types

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
## and because it is unsigned, its lowest value is 0. This means the 256 numbers
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
## or too small to fit in that range (e.g. calling `Int.highestI32 + 1`),
## then the operation will *overflow*. When an overflow occurs, the program will crash.
##
## As such, it's very important to design your code not to exceed these bounds!
## If you need to do math outside these bounds, consider using a larger numeric size.
# Int size : Num [ @Int size ]

## Arithmetic

## Divide two integers and discard any fractional part of the result.
##
## Division by zero is undefined in mathematics. As such, you should make
## sure never to pass zero as the denomaintor to this function!
##
## If zero does get passed as the denominator...
##
## * In a development build, you'll get an assertion failure.
## * In an optimized build, the function will return 0.
##
## `a // b` is shorthand for `Int.div a b`.
##
## >>> 5 // 7
##
## >>> Int.div 5 7
##
## >>> 8 // -3
##
## >>> Int.div 8 -3
##
## This is the same as the #// operator.
# div : Int, Int -> Int

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

#bitwiseXor : Int -> Int -> Int

#bitwiseAnd : Int -> Int -> Int

#bitwiseNot : Int -> Int

## Sort ascending - that is, with the lowest first, and the highest last.
##
##     List.sort Int.asc [ 3, 6, 0 ]
##
asc : Int a, Int a -> [ Eq, Lt, Gt ]

## Sort descending - that is, with the highest first, and the lowest last.
##
##     List.sort Int.desc [ 3, 6, 0 ]
##
desc : Int a, Int a -> [ Eq, Lt, Gt ]

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
## Note that this is smaller than the positive version of #Int.lowestI32
## which means if you call #Num.abs on #Int.lowestI32, it will overflow and crash!
maxI32 : I32

## The lowest number that can be stored in an #I32 without overflowing its
## available memory and crashing.
##
## Note that the positive version of this number is this is larger than
## #Int.highestI32, which means if you call #Num.abs on #Int.lowestI32, it will overflow and crash!
minI32 : I32
