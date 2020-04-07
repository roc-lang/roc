interface Real
    exposes [
        Real,
        fromNum,
        round,
        ceiling,
        floor,
        div,
        mod,
        recip,
        sqrt,
        maxF32,
        maxF64,
        maxRatio,
        minF32,
        minF64,
        minReal,
        sin,
        cos,
        tan,
        asin,
        acos,
        atan
    ]
    imports []

## Types

## A [real number](https://en.wikipedia.org/wiki/Real_number). All number literals with decimal points are #Real values.
##
## ## Precision
##
## Every programming language is bound by finite hardware, which means numbers
## in every programming language have necessarily limited precision. (For example, imagine
## trying to work with the individual digits in a quintillion raised to itself
## a quintillion times over. If that sounds physically representable, you can repeat
## that operation until it isn't; eventually there aren't enough atoms in the universe
## to possibly represent all the digits. Math is bigger than the physical reality in which hardware exists!)
##
## For this reason, Roc has limited precision for storing reals.
##
## Rational reals like `-3.0` and `1/3` can be represented precisely in the #Ratio type, up
## to a certain size. Irrational reals, such as [π](https://en.wikipedia.org/wiki/Pi) or
## the square root of 2, are approximated.
##
## >>> 0.1
##
## >>> 1.0
##
## >>> 0.0
##
## If you like, you can put underscores in your #Real literals.
## They have no effect on the number's value, but can make things easier to read.
##
## >>> 1_000_000.000_000_001
##
## A #Real values can be either a *floating-point* values or a #Ratio value.
## Floating-point values (#F16, #F32, #F64, or #F128) commmonly have hardware
## support, making them run faster than #Ratio. However, #Ratio is more precise
## for arithmetic instructions.
##
## For example, compare the outputs of adding 0.1 and 0.2 in ratios compared to floats:
##
## >>> 0.1r + 0.2r
##
## >>> 0.1f64 + 0.2f64
##
## Floating point values work this way because of some (very reasonable)
## hardware design decisions made in 1985, which are hardwired into all modern processors.
## The performance penalty for having things work any other way than this is severe, so
## for performance-critical code, it is generally best to choose floats over ratios.
## In contrast, for precision-critical code (like anything dealing with money),
## it's generally best to choose ratios over floats.
##
## Like #Int, it's possible for #Real operations to overflow.
##
## Although some languages treat have first-class representations for
## `-Infinity`, `Infinity`, and the special `NaN` ("not a number")
## floating-point values described in the IEEE-754, Roc does not.
## Instead, Roc treats all of these as errors. If any floating-point operation
## in encounters one of these values, it will deal with the error in some other way.
##
## ## Loud versus Quiet errors
##
## Besides precision problems, another reason floats are error-prone
## is that they have quiet error handling built in. For example, in
## a 64-bit floating point number, there are certain patterns of those
## 64 bits which do not represent valid floats; instead, they represent
## erroneous results of previous operations.
##
## Whenever any arithmetic operation is performed on an erroneous float,
## the result is also erroneous. This is called *error propagation*, and
## it is notoriously error-prone. In Roc, using equality operations like
## `==` and `!=` on an erroneous float causes a crash. (See #Float.isErroneous
## for other ways to check what erroneous value you have.)
##
## Beause erroneous floats are so error-prone, Roc discourages using them.
## Instead, by default it treats them the same way as overflow: by
## crashing whenever any #Float function would otherwise return one.
## You can also use functions like #Float.tryAdd to get an `Ok` or an error
## back so you can gracefully recover from erroneous values.
##
## Quiet errors can be useful sometimes. For example, you might want to
## do three floating point calculations in a row, and then gracefully handle
## the situation where any one of the three was erroneous. In that situation,
## quiet errors can be more efficient than using three `try` functions, because
## it can have one condition at the end instead of three along the way.
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

toF32 : Num * -> F64
toF64 : Num * -> F64
toRatio : Num * -> Ratio

round : Real * -> Int

ceil : Real * -> Int

floor : Real * -> Int

## Trigonometry

## Return an approximation of a number's cosine.
##
## For #Ratio values, this will convert to #F64, perform the operation, and
## convert back to #Ratio. So calling `Real.cos ratio` is the same as calling
## `Real.toRatio (Real.cos (Real.toF64 ratio))`.
cos : Real a -> Real a

acos : Real a -> Real a

sin : Real a -> Real a

asin : Real a -> Real a

tan : Real a -> Real a

atan : Real a -> Real a

## Other Calculations (arithmetic?)

## Divide two #Real numbers.
##
## `a / b` is shorthand for `Real.div a b`.
##
## Division by zero is undefined in mathematics. As such, you should make
## sure never to pass zero as the denomaintor to this function!
##
## If zero does get passed as the denominator...
##
## * In a development build, you'll get an assertion failure.
## * In a release build, the function will return `Infinity`, `-Infinity`, or `NaN` depending on the arguments.
##
## To divide an #Int and a #Real, first convert the #Int to a #Real using one of the functions in this module.
##
## >>> 5.0 / 7.0
##
## >>> Real.div 5 7
##
## `Real.div` can be convenient in pipelines.
##
## >>> Real.piF64
## >>>     |> Real.div 2.0
div : Real a, Real a -> Result Real DivByZero

## Perform modulo on two #Real numbers.
##
## Modulo is the same as remainder when working with positive numbers,
## but if either number is negative, then modulo works differently.
##
## Return `Err DivByZero` if the second number is zero, because division by zero is undefined in mathematics.
##
## `a % b` is shorthand for `Real.mod a b`.
##
## >>> 5.0 % 7.0
##
## >>> Real.mod 5 7
##
## `Real.mod` can be convenient in pipelines.
##
## >>> Real.piF64
## >>>     |> Real.mod 2.0
mod : Real a, Real a -> Result Real DivByZero

tryMod : Real a, Real a -> Result (Real a) [ DivByZero ]*

## Return the reciprocal of a #Real - that is, divides `1.0` by the given number.
##
## Crashes if given `0.0`, because division by zero is undefined in mathematics.
##
## For a version that does not crash, use #tryRecip
recip : Real a -> Result (Real a) [ DivByZero ]*


tryRecip : Real a -> Result (Real a) [ DivByZero ]*

## Return an approximation of the absolute value of the square root of the #Real.
##
## Return #InvalidSqrt if given a negative number or an erroneous #Real. The square root of a negative number is an [imaginary number](https://en.wikipedia.org/wiki/Imaginary_number), which is not one of the real numbers.
##
## >>> Real.sqrt 4.0
##
## >>> Real.sqrt 1.5
##
## >>> Real.sqrt 0.0
##
## >>> Real.sqrt -4.0
sqrt : Real a -> [Ok (Real a), InvalidSqrt]*

## Like #Real.sqrt, but returning a *quiet NaN* if given a negative number.
##
## Quiet NaNs are notoriously more error-prone than explicit #Ok unions,
## so if you're using this instead of #Real.sqrt, be very careful not to let
## potential error cases go unhandled.
##
## ## Performance Notes
##
## This runs faster than #Real.sqrt, but is more error-prone because it makes
## it easier to forget to handle potential error cases. You may not forget
## when you just got done reading this paragraph, but the next person who
## comes along to modify the code may not have read it at all, and might not
## realize the need for seurity checks beause the requirement is implicit.
sqrtQuiet : Real a -> Real a

## Constants

## An approximation of [e](https://en.wikipedia.org/wiki/E_(mathematical_constant))
## in F64 form, specifically 2.718281828459045.
eF64 : F64

## An approximation of [pi](https://en.wikipedia.org/wiki/Pi)
## in F64 form, specifically 3.141592653589793.
piF64 : F64

## Sort ascending - that is, with the lowest first, and the highest last.
##
##     List.sort Real.asc [ 3.0, 6.0, 0.0 ]
##
asc : Real a, Real a -> [ Eq, Lt, Gt ]

## Sort descending - that is, with the highest first, and the lowest last.
##
##     List.sort Real.desc [ 3.0, 6.0, 0.0 ]
##
desc : Real a, Real a -> [ Eq, Lt, Gt ]

## Limits

## The highest supported #F64 value you can have, which is approximately 1.8 × 10^308.
##
## If you go higher than this, your running Roc code will crash - so be careful not to!
maxF64 : F64

## The lowest supported #F64 value you can have, which is approximately -1.8 × 10^308.
##
## If you go lower than this, your running Roc code will crash - so be careful not to!
minF64 : F64

## The highest integer that can be represented as a #F64 without # losing precision.
## It is equal to 2^53, which is approximately 9 × 10^15.
##
## Some integers higher than this can be represented, but they may lose precision. For example:
##
## >>> Real.highestInt
##
## >>> Real.highestInt + 100 # Increasing may lose precision
##
## >>> Real.highestInt - 100 # Decreasing is fine - but watch out for lowestLosslessInt!
maxIntF64 : Real *

## The lowest integer that can be represented as a #F64 without losing precision.
## It is equal to -2^53, which is approximately -9 × 10^15.
##
## Some integers lower than this can be represented, but they may lose precision. For example:
##
## >>> Real.lowestIntVal
##
## >>> Real.lowestIntVal - 100 # Decreasing may lose precision
##
## >>> Real.lowestIntVal + 100 # Increasing is fine - but watch out for highestInt!
minIntF64 : Real *
