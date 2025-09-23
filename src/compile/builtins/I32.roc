module [
    I32,
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

I32 := {}

## Addition for 32-bit signed integers (builtin implementation)
##
## ```
## expect 12 + 12 == I32.plus(12,12)
## ```
plus : I32, I32 -> I32

## Subtraction for 32-bit signed integers (builtin implementation)
##
## ```
## expect 25 - 12 == I32.minus(25,12)
## ```
minus : I32, I32 -> I32

## Multiplication for 32-bit signed integers (builtin implementation)
##
## ```
## expect 12 * 3 == I32.times(12,3)
## ```
times : I32, I32 -> I32

## Division for 32-bit signed integers (builtin implementation)
##
## ```
## expect 36 / 4 == I32.div_by(36,4)
## ```
div_by : I32, I32 -> I32

## Truncated division for 32-bit signed integers (builtin implementation)
##
## ```
## expect -37 // 4 == I32.div_trunc_by(-37,4)
## ```
div_trunc_by : I32, I32 -> I32

## Exponentiation for 32-bit signed integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == I32.raise_to(3,4)
## ```
raise_to : I32, I32 -> I32

## Remainder for 32-bit signed integers (builtin implementation)
##
## ```
## expect -37 % 4 == I32.rem_by(-37,4)
## ```
rem_by : I32, I32 -> I32

## Negation for 32-bit signed integers (builtin implementation)
##
## ```
## expect -12 == I32.negate(12)
## ```
negate : I32 -> I32

## Equality for 32-bit signed integers (builtin implementation)
##
## ```
## expect (12 == 12) == I32.equals(12,12)
## ```
equals : I32, I32 -> Bool

## Inequality for 32-bit signed integers (builtin implementation)
##
## ```
## expect (12 != 13) == I32.not_equals(12,13)
## ```
not_equals : I32, I32 -> Bool
