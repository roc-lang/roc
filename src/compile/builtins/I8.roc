module [
    I8,
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

I8 := {}

## Addition for 8-bit signed integers (builtin implementation)
##
## ```
## expect 12 + 12 == I8.plus(12,12)
## ```
plus : I8, I8 -> I8

## Subtraction for 8-bit signed integers (builtin implementation)
##
## ```
## expect 25 - 12 == I8.minus(25,12)
## ```
minus : I8, I8 -> I8

## Multiplication for 8-bit signed integers (builtin implementation)
##
## ```
## expect 12 * 3 == I8.times(12,3)
## ```
times : I8, I8 -> I8

## Division for 8-bit signed integers (builtin implementation)
##
## ```
## expect 36 / 4 == I8.div_by(36,4)
## ```
div_by : I8, I8 -> I8

## Truncated division for 8-bit signed integers (builtin implementation)
##
## ```
## expect -37 // 4 == I8.div_trunc_by(-37,4)
## ```
div_trunc_by : I8, I8 -> I8

## Exponentiation for 8-bit signed integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == I8.raise_to(3,4)
## ```
raise_to : I8, I8 -> I8

## Remainder for 8-bit signed integers (builtin implementation)
##
## ```
## expect -37 % 4 == I8.rem_by(-37,4)
## ```
rem_by : I8, I8 -> I8

## Negation for 8-bit signed integers (builtin implementation)
##
## ```
## expect -12 == I8.negate(12)
## ```
negate : I8 -> I8

## Equality for 8-bit signed integers (builtin implementation)
##
## ```
## expect (12 == 12) == I8.equals(12,12)
## ```
equals : I8, I8 -> Bool

## Inequality for 8-bit signed integers (builtin implementation)
##
## ```
## expect (12 != 13) == I8.not_equals(12,13)
## ```
not_equals : I8, I8 -> Bool
