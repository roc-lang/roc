api Num provides Num, negate

## Types

## Represents a number that could be either an @Int or a @Float.
##
## This is useful for functions that can work on either, for example @Num.add, whose type is:
##
## ```
## add : Num range, Num range -> Num range
## ```
##
## The number 1.5 technically has the type `Num FloatingPoint`, so when you pass two of them to `Num.add`, the answer you get is `3.0 : Num FloatingPoint`.
##
## The type @Float is defined to be an alias for `Num FloatingPoint`, so `3.0 : Num FloatingPoint` is the same answer as `3.0 : Float`.
##
## Similarly, the number 1 technically has the type `Num Integer`, so when you pass two of them to `Num.add`, the answer you get is `2 : Num Integer`.
##
## The type @Int is defined to be an alias for `Num Integer`, so `2 : Num Integer` is the same answer as `2 : Int`.
##
## In this way, the `Num` type makes it possible to have `1 + 1` return `2 : Int` and `1.5 + 1.5` return `3.0 : Float`.
Num range := Num range

## Returned by division operations (@Float.div, @Float.mod, @Int.divFloor, @Int.divMod, and @Float.recip) when they attempt to divide by zero.
DivisionByZero := DivisionByZero

## Convert

## Return a negative number when given a positive one, and vice versa.
##
## Some languages have a unary `-` operator (for example, `-(a + b)`), but Roc does not. If you want to negate a number, calling this function is the way to do it!
##
## > Num.negate 5
##
## > Num.negate -2.5
##
## > Num.negate 0
##
## > Num.negate 0.0
##
## This will crash when given @Int.lowestValue, because doing so will result in a number higher than @Int.highestValue.
##
## (It will never crash when given a @Float, however, because of how floating point numbers represent positive and negative numbers.)
negate : Num range -> Num range

## Return the absolute value of the number.
##
## * For a positive number, returns the same number.
## * For a negative number, returns the same number except positive.
##
## > Num.abs 4
##
## > Num.abs -2.5
##
## > Num.abs 0
##
## > Num.abs 0.0
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
## (To add an @Int and a @Float, first convert one so that they both have the same type. There are functions in the [`Float`](/Float) module that can convert both @Int to @Float and the other way around.)
##
## `a + b` is shorthand for `Num.add a b`.
##
## > 5 + 7
##
## > Num.add 5 7
##
## > 3.0 + -0.5
##
## > Num.add 3.0 -0.5
add : Num range, Num range -> Num range

## Subtract two numbers of the same type.
##
## (To subtract an @Int and a @Float, first convert one so that they both have the same type. There are functions in the [`Float`](/Float) module that can convert both @Int to @Float and the other way around.)
##
## `a - b` is shorthand for `Num.sub a b`.
##
## > 7 - 5
##
## > Num.sub 7 5
##
## > 3.0 - -0.5
##
## > Num.sub 3.0 -0.5
sub : Num range, Num range -> Num range

## Multiply two numbers of the same type.
##
## (To multiply an @Int and a @Float, first convert one so that they both have the same type. There are functions in the [`Float`](/Float) module that can convert both @Int to @Float and the other way around.)
##
## `a * b` is shorthand for `Num.mul a b`.
##
## > 5 * 7
##
## > Num.mul 5 7
##
## > 4.0 * -0.5
##
## > Num.mul 4.0 -0.5
mul : Num range, Num range -> Num range
