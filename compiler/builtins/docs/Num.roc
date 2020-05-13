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
## Only #Float values will show a decimal point, and they will always have one.
##
## >>> Num.toStr 4.2
##
## >>> Num.toStr 4.0
##
## For other bases see #toHexStr, #toOctalStr, and #toBinaryStr.
toStr : Num * -> Str

