interface Float
    exposes [ Float, FloatingPoint, InvalidSqrt, fromNum, round, ceiling, floor,
        div, mod, recip, sqrt, highestVal, lowestVal, highestIntVal, lowestIntVal,
        sin, cos, tan, asin, acos, atan ]
    imports []

## Types

## A 64-bit floating-point number. All number literals with decimal points are @Float values.
##
## > 0.1
##
## > 1.0
##
## > 0.0
##
## If you like, you can put underscores in your @Float literals.
## They have no effect on the number's value, but can make things easier to read.
##
## > 1_000_000.000_000_001
##
## Unlike @Int values, @Float values are imprecise. A classic example of this imprecision:
##
## > 0.1 + 0.2
##
## Floating point values work this way because of some (very reasonable) hardware design decisions made in 1985, which are hardwired into all modern CPUs. The performance penalty for having things work any other way than this is severe, so Roc defaults to using the one with hardware support.
##
## It is possible to build fractional systems with different precision characteristics (for example, storing an @Int numerator and @Int denominator), but their basic arithmetic operations will be unavoidably slower than @Float.
##
## See @Float.highestSupported and @Float.lowestSupported for the highest and
## lowest values that can be held in a @Float.
##
## Note that although the IEEE-754 specification describes the values `Infinity`, `-Infinity`, `NaN`, and `-0.0`, Roc avoids these as follows:
##
## * @Float.sqrt returns `Err InvalidSqrt` when it would otherwise return `NaN`.
## * Division operations return `Err DivByZero` when they would otherwise return `Infinity` or `-Infinity`.
## * Operations that overflow crash (just like integers do) instead of returning `Infinity` or `-Infinity`.
## Under the hood, it is possible to have a zero @Float with a negative sign. However, this implementation detail intentionally conceealed. For equality purpose, `-0.0` is treated as equivalent to `0.0`, just like the spec prescribes. However, @Str.decimal always returns `0.0` when it would otherwise return `-0.0`, and both @Num.isPositive and @Num.isNegative return @False for all zero values. The only way to detect a zero with a negative sign is to convert it to @Bytes and inspect the bits directly.
Float : Num FloatingPoint

FloatingPoint := FloatingPoint

## Returned by @Float.sqrt when given a negative number.
InvalidSqrt := InvalidSqrt

## Conversions

fromNum : Num * -> Float

round : Float -> Int

ceiling : Float -> Int

floor : Float -> Int

## Trigonometry

cos : Float -> Float

acos : Float -> Float

sin : Float -> Float

asin : Float -> Float

tan : Float -> Float

atan : Float -> Float

## Other Calculations (arithmetic?)

## Divide two @Float numbers. Return `Err DivByZero` if the
## second number is zero, because division by zero is undefined in mathematics.
##
## (To divide an @Int and a @Float, first convert the @Int to a @Float using one of the functions in this module.)
##
## `a / b` is shorthand for `Float.div a b`.
##
## > 5.0 / 7.0
##
## > Float.div 5 7
##
## > 4.0 / -0.5
##
## > Float.div 4.0 -0.5
div : Float, Float -> Result Float DivByZero

## Perform modulo on two @Float numbers.
##
## Modulo is the same as remainder when working with positive numbers,
## but if either number is negative, then modulo works differently.
##
## Return `Err DivByZero` if the second number is zero, because division by zero is undefined in mathematics.
##
## `a % b` is shorthand for `Float.mod a b`.
##
## > 5.0 % 7.0
##
## > Float.mod 5 7
##
## > 4.0 % -0.5
##
## > Float.mod -8 -3
mod : Float, Float -> Result Float DivByZero

## Return the reciprocal of the @Float.
recip : Float -> Result Float Num.DivByZero
recip = \float ->
    1.0 / float

## Return an approximation of the absolute value of the square root of the @Float.
##
## Return @InvalidSqrt if given a negative number. The square root of a negative number is an irrational number, and @Float only supports rational numbers.
##
## > Float.sqrt 4.0
##
## > Float.sqrt 1.5
##
## > Float.sqrt 0.0
##
## > Float.sqrt -4.0
sqrt : Float -> Result Float InvalidSqrt

## Constants

## An approximation of e, specifically 2.718281828459045.
e : Float
e = 2.718281828459045

## An approximation of pi, specifically 3.141592653589793.
pi : Float
pi = 3.141592653589793

## Limits

## The highest supported @Float value you can have, which is approximately 1.8 × 10^308.
##
## If you go higher than this, your running Roc code will crash - so be careful not to!
highestVal : Float

## The lowest supported @Float value you can have, which is approximately -1.8 × 10^308.
##
## If you go lower than this, your running Roc code will crash - so be careful not to!
lowestVal : Float

## The highest integer that can be represented as a @Float without # losing precision. 
## It is equal to 2^53, which is approximately 9 × 10^15.
##
## Some integers higher than this can be represented, but they may lose precision. For example:
##
## > Float.highestIntVal
##
## > Float.highestIntVal + 100 # Increasing may lose precision
##
## > Float.highestIntVal - 100 # Decreasing is fine - but watch out for lowestLosslessInt!
highestIntVal : Float

## The lowest integer that can be represented as a @Float without losing precision. 
## It is equal to -2^53, which is approximately -9 × 10^15.
##
## Some integers lower than this can be represented, but they may lose precision. For example:
##
## > Float.lowestIntVal
##
## > Float.lowestIntVal - 100 # Decreasing may lose precision
##
## > Float.lowestIntVal + 100 # Increasing is fine - but watch out for highestLosslessInt!
lowestIntVal : Float
