module [
    I128,
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

I128 := {}

## Addition for 128-bit signed integers (builtin implementation)
##
## ```
## expect 12 + 12 == I128.plus(12,12)
## ```
plus : I128, I128 -> I128

## Subtraction for 128-bit signed integers (builtin implementation)
##
## ```
## expect 25 - 12 == I128.minus(25,12)
## ```
minus : I128, I128 -> I128

## Multiplication for 128-bit signed integers (builtin implementation)
##
## ```
## expect 12 * 3 == I128.times(12,3)
## ```
times : I128, I128 -> I128

## Division for 128-bit signed integers (builtin implementation)
##
## ```
## expect 36 / 4 == I128.div_by(36,4)
## ```
div_by : I128, I128 -> I128

## Truncated division for 128-bit signed integers (builtin implementation)
##
## ```
## expect -37 // 4 == I128.div_trunc_by(-37,4)
## ```
div_trunc_by : I128, I128 -> I128

## Exponentiation for 128-bit signed integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == I128.raise_to(3,4)
## ```
raise_to : I128, I128 -> I128

## Remainder for 128-bit signed integers (builtin implementation)
##
## ```
## expect -37 % 4 == I128.rem_by(-37,4)
## ```
rem_by : I128, I128 -> I128

## Negation for 128-bit signed integers (builtin implementation)
##
## ```
## expect -12 == I128.negate(12)
## ```
negate : I128 -> I128

## Equality for 128-bit signed integers (builtin implementation)
##
## ```
## expect (12 == 12) == I128.equals(12,12)
## ```
equals : I128, I128 -> Bool

## Inequality for 128-bit signed integers (builtin implementation)
##
## ```
## expect (12 != 13) == I128.not_equals(12,13)
## ```
not_equals : I128, I128 -> Bool
