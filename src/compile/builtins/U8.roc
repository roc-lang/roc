module [
    U8,
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

U8 := {}

## Addition for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 + 12 == U8.plus(12,12)
## ```
plus : U8, U8 -> U8

## Subtraction for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect 25 - 12 == U8.minus(25,12)
## ```
minus : U8, U8 -> U8

## Multiplication for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 * 3 == U8.times(12,3)
## ```
times : U8, U8 -> U8

## Division for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect 36 / 4 == U8.div_by(36,4)
## ```
div_by : U8, U8 -> U8

## Truncated division for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 // 4 == U8.div_trunc_by(37,4)
## ```
div_trunc_by : U8, U8 -> U8

## Exponentiation for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == U8.raise_to(3,4)
## ```
raise_to : U8, U8 -> U8

## Remainder for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 % 4 == U8.rem_by(37,4)
## ```
rem_by : U8, U8 -> U8

## Negation for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect -12 == U8.negate(12)
## ```
negate : U8 -> U8

## Equality for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 == 12) == U8.equals(12,12)
## ```
equals : U8, U8 -> Bool

## Inequality for 8-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 != 13) == U8.not_equals(12,13)
## ```
not_equals : U8, U8 -> Bool
