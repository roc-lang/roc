module [
    U128,
    plus,
    minus,
    times,
    div_by,
    div_trunc_by,
    raise_to,
    rem_by,
    negate,
    equals,
    not_equals,
]

U128 := {}

## Addition for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 + 12 == U128.plus(12,12)
## ```
plus : U128, U128 -> U128

## Subtraction for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect 25 - 12 == U128.minus(25,12)
## ```
minus : U128, U128 -> U128

## Multiplication for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 * 3 == U128.times(12,3)
## ```
times : U128, U128 -> U128

## Division for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect 36 / 4 == U128.div_by(36,4)
## ```
div_by : U128, U128 -> U128

## Truncated division for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 // 4 == U128.div_trunc_by(37,4)
## ```
div_trunc_by : U128, U128 -> U128

## Exponentiation for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == U128.raise_to(3,4)
## ```
raise_to : U128, U128 -> U128

## Remainder for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 % 4 == U128.rem_by(37,4)
## ```
rem_by : U128, U128 -> U128

## Negation for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect -12 == U128.negate(12)
## ```
negate : U128 -> U128

## Equality for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 == 12) == U128.equals(12,12)
## ```
equals : U128, U128 -> Bool

## Inequality for 128-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 != 13) == U128.not_equals(12,13)
## ```
not_equals : U128, U128 -> Bool
