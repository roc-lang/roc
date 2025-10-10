module [
    U64,
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

U64 := {}

## Addition for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 + 12 == U64.plus(12,12)
## ```
plus : U64, U64 -> U64

## Subtraction for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect 25 - 12 == U64.minus(25,12)
## ```
minus : U64, U64 -> U64

## Multiplication for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 * 3 == U64.times(12,3)
## ```
times : U64, U64 -> U64

## Division for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect 36 / 4 == U64.div_by(36,4)
## ```
div_by : U64, U64 -> U64

## Truncated division for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 // 4 == U64.div_trunc_by(37,4)
## ```
div_trunc_by : U64, U64 -> U64

## Exponentiation for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == U64.raise_to(3,4)
## ```
raise_to : U64, U64 -> U64

## Remainder for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 % 4 == U64.rem_by(37,4)
## ```
rem_by : U64, U64 -> U64

## Negation for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect -12 == U64.negate(12)
## ```
negate : U64 -> U64

## Equality for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 == 12) == U64.equals(12,12)
## ```
equals : U64, U64 -> Bool

## Inequality for 64-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 != 13) == U64.not_equals(12,13)
## ```
not_equals : U64, U64 -> Bool
