interface Float
    exposes [
        Float,
        fromNum,
        round,
        ceiling,
        floor,
        div,
        mod,
        recip,
        sqrt,
        highest,
        lowest,
        highestInt,
        lowestInt,
        sin,
        cos,
        tan,
        asin,
        acos,
        atan
    ]
    imports []

## Types

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
## This specification covers these float formats, all of which Roc supports:
##
## - #F16 (16-bit binary float) # TODO show a table like we do with ints, with the min/max ranges
## - #F32 (32-bit binary float)
## - #F64 (64-bit binary float)
## - #F128 (128-bit binary float)
## - #D32 (32-bit decimal float)
## - #D64 (64-bit decimal float)
## - #D128 (128-bit decimal float)
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

## Conversions

#fromNum : Num * -> Float

#round : Float -> Int
round = \num ->
    when num is
        0.0 -> 0
        _ -> 1

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

## Like #Float.sqrt, but returning a *quiet NaN* if given a negative number.
##
## Quiet NaNs are notoriously more error-prone than explicit #Ok unions,
## so if you're using this instead of #Float.sqrt, be very careful not to let
## potential error cases go unhandled.
##
## ## Performance Notes
##
## This runs faster than #Float.sqrt, but is more error-prone because it makes
## it easier to forget to handle potential error cases. You may not forget
## when you just got done reading this paragraph, but the next person who
## comes along to modify the code may not have read it at all, and might not
## realize the need for seurity checks beause the requirement is implicit.
sqrtQuiet : Float a -> Float a

## Constants

## An approximation of e, specifically 2.718281828459045.
e : Float *

## An approximation of pi, specifically 3.141592653589793.
pi : Float *

## Sort ascending - that is, with the lowest first, and the highest last.
##
##     List.sort Float.asc [ 3.0, 6.0, 0.0 ]
##
asc : Float a, Float a -> [ Eq, Lt, Gt ]

## Sort descending - that is, with the highest first, and the lowest last.
##
##     List.sort Float.desc [ 3.0, 6.0, 0.0 ]
##
desc : Float a, Float a -> [ Eq, Lt, Gt ]

## Any float that resulted from a quiet operation may be invalid.
## This verifies whether they are still valid or have become invalid.
##
## >>> Float.verify (Float.quietSqrt -2)
##
## >>> Float.verify (Float.quietSqrt 2)
##
## >>> Float.verify (Float.quietDiv 1 0)
##
## >>> Float.verify (Float.quietDiv -1 0)
##
## Note that even if you personally never use *quiet* operations, any float
## you get from outside your code base, such as the host or a third-party module,
## may be invalid.
verify : Float * -> [ Valid, Infinity, MinusInfinity, NaN ]

## Any float that resulted from a quiet operation may be invalid.
## This returns `True` if the float is still valid.
##
## >>> Float.isValid (Float.quietSqrt -2)
##
## >>> Float.isValid (Float.quietSqrt 2)
##
## >>> Float.isValid (Float.quietDiv 1 0)
##
## >>> Float.isValid (Float.quietDiv -1 0)
##
## This will return `True` if calling #Float.verify on this float returns `Valid`,
## and will return `False` otherwise.
##
## Note that even if you personally never use *quiet* operations, any float
## you get from outside your code base, such as the host or a third-party module,
## may be invalid.
isValid : Float * -> Bool

## Limits

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
