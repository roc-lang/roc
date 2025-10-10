module [
    U16,
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

U16 := {}

## Addition for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 + 12 == U16.plus(12,12)
## ```
plus : U16, U16 -> U16

## Subtraction for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect 25 - 12 == U16.minus(25,12)
## ```
minus : U16, U16 -> U16

## Multiplication for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 * 3 == U16.times(12,3)
## ```
times : U16, U16 -> U16

## Division for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect 36 / 4 == U16.div_by(36,4)
## ```
div_by : U16, U16 -> U16

## Truncated division for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 // 4 == U16.div_trunc_by(37,4)
## ```
div_trunc_by : U16, U16 -> U16

## Exponentiation for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == U16.raise_to(3,4)
## ```
raise_to : U16, U16 -> U16

## Remainder for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 % 4 == U16.rem_by(37,4)
## ```
rem_by : U16, U16 -> U16

## Negation for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect -12 == U16.negate(12)
## ```
negate : U16 -> U16

## Equality for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 == 12) == U16.equals(12,12)
## ```
equals : U16, U16 -> Bool

## Inequality for 16-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 != 13) == U16.not_equals(12,13)
## ```
not_equals : U16, U16 -> Bool
