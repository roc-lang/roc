module [
    U32,
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

U32 := {}

## Addition for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 + 12 == U32.plus(12,12)
## ```
plus : U32, U32 -> U32

## Subtraction for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect 25 - 12 == U32.minus(25,12)
## ```
minus : U32, U32 -> U32

## Multiplication for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect 12 * 3 == U32.times(12,3)
## ```
times : U32, U32 -> U32

## Division for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect 36 / 4 == U32.div_by(36,4)
## ```
div_by : U32, U32 -> U32

## Truncated division for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 // 4 == U32.div_trunc_by(37,4)
## ```
div_trunc_by : U32, U32 -> U32

## Exponentiation for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == U32.raise_to(3,4)
## ```
raise_to : U32, U32 -> U32

## Remainder for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect 37 % 4 == U32.rem_by(37,4)
## ```
rem_by : U32, U32 -> U32

## Negation for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect -12 == U32.negate(12)
## ```
negate : U32 -> U32

## Equality for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 == 12) == U32.equals(12,12)
## ```
equals : U32, U32 -> Bool

## Inequality for 32-bit unsigned integers (builtin implementation)
##
## ```
## expect (12 != 13) == U32.not_equals(12,13)
## ```
not_equals : U32, U32 -> Bool
