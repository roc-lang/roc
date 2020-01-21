interface Int
    exposes [ Int ]
    imports []

## Types

## An integer value. All number literals without decimal points are #Int values.
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
## * CPUs typically work fastest on their native [word size](https://en.wikipedia.org/wiki/Word_(computer_architecture)). For example, 64-bit CPUs tend to work fastest on 64-bit integers. Especially if your performance profiling shows that you are CPU bound rather than memory bound, consider #Iword or #Uword.
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
## There are also two variable-size integer types: #Iword and #Uword.
## Their sizes are determined by the machine word size for the system you're
## compiling for. For example, on a 64-bit system, #Iword is the same as #I64,
## and #Uword is the same as #U64.
##
## If any operation would result in an #Int that is either too big
## or too small to fit in that range (e.g. running `Int.highest + 1`),
## then the operation will overflow. When this happens:
##
## * In a development build, you'll get an assertion failure.
## * In a release build, you'll get [wrapping overflow](https://en.wikipedia.org/wiki/Integer_overflow), which is almost always a mathematically incorrect outcome for the requested operation. (If you actually want wrapping, because you're writing something like a hash function, use functions like #Int.addWrapping.)
##
## As such, it's very important to design your code not to exceed these bounds!
## If you need to do math outside these bounds, consider using
## a different representation other than #Int. The reason #Int has these
## bounds is for performance reasons.
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

## Limits

## The highest number that can be stored in an #Int without overflowing its
## available memory (64 bits total) and crashing.
##
## Note that this is smaller than the positive version of #Int.lowest,
## which means if you call #Num.abs on #Int.lowest, it will crash!
#highest : Int
highest = 0x7fff_ffff_ffff_ffff

## The lowest number that can be stored in an #Int without overflowing its
## available memory (64 bits total) and crashing.
##
## Note that the positive version of this number is this is larger than
## #Int.highest, which means if you call #Num.abs on #Int.lowest,
## it will crash!
#lowest : Int
lowest = -0x8000_0000_0000_0000
