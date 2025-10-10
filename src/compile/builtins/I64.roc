module [
    I64,
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

I64 := {}

## Addition for 64-bit signed integers (builtin implementation)
##
## ```
## expect 12 + 12 == I64.plus(12,12)
## ```
plus : I64, I64 -> I64

## Subtraction for 64-bit signed integers (builtin implementation)
##
## ```
## expect 25 - 12 == I64.minus(25,12)
## ```
minus : I64, I64 -> I64

## Multiplication for 64-bit signed integers (builtin implementation)
##
## ```
## expect 12 * 3 == I64.times(12,3)
## ```
times : I64, I64 -> I64

## Division for 64-bit signed integers (builtin implementation)
##
## ```
## expect 36 / 4 == I64.div_by(36,4)
## ```
div_by : I64, I64 -> I64

## Truncated division for 64-bit signed integers (builtin implementation)
##
## ```
## expect -37 // 4 == I64.div_trunc_by(-37,4)
## ```
div_trunc_by : I64, I64 -> I64

## Exponentiation for 64-bit signed integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == I64.raise_to(3,4)
## ```
raise_to : I64, I64 -> I64

## Remainder for 64-bit signed integers (builtin implementation)
##
## ```
## expect -37 % 4 == I64.rem_by(-37,4)
## ```
rem_by : I64, I64 -> I64

## Negation for 64-bit signed integers (builtin implementation)
##
## ```
## expect -12 == I64.negate(12)
## ```
negate : I64 -> I64

## Equality for 64-bit signed integers (builtin implementation)
##
## ```
## expect (12 == 12) == I64.equals(12,12)
## ```
equals : I64, I64 -> Bool

## Inequality for 64-bit signed integers (builtin implementation)
##
## ```
## expect (12 != 13) == I64.not_equals(12,13)
## ```
not_equals : I64, I64 -> Bool
