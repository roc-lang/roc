module [
    I16,
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

I16 := {}

## Addition for 16-bit signed integers (builtin implementation)
##
## ```
## expect 12 + 12 == I16.plus(12,12)
## ```
plus : I16, I16 -> I16

## Subtraction for 16-bit signed integers (builtin implementation)
##
## ```
## expect 25 - 12 == I16.minus(25,12)
## ```
minus : I16, I16 -> I16

## Multiplication for 16-bit signed integers (builtin implementation)
##
## ```
## expect 12 * 3 == I16.times(12,3)
## ```
times : I16, I16 -> I16

## Division for 16-bit signed integers (builtin implementation)
##
## ```
## expect 36 / 4 == I16.div_by(36,4)
## ```
div_by : I16, I16 -> I16

## Truncated division for 16-bit signed integers (builtin implementation)
##
## ```
## expect -37 // 4 == I16.div_trunc_by(-37,4)
## ```
div_trunc_by : I16, I16 -> I16

## Exponentiation for 16-bit signed integers (builtin implementation)
##
## ```
## expect 3 ^ 4 == I16.raise_to(3,4)
## ```
raise_to : I16, I16 -> I16

## Remainder for 16-bit signed integers (builtin implementation)
##
## ```
## expect -37 % 4 == I16.rem_by(-37,4)
## ```
rem_by : I16, I16 -> I16

## Negation for 16-bit signed integers (builtin implementation)
##
## ```
## expect -12 == I16.negate(12)
## ```
negate : I16 -> I16

## Equality for 16-bit signed integers (builtin implementation)
##
## ```
## expect (12 == 12) == I16.equals(12,12)
## ```
equals : I16, I16 -> Bool

## Inequality for 16-bit signed integers (builtin implementation)
##
## ```
## expect (12 != 13) == I16.not_equals(12,13)
## ```
not_equals : I16, I16 -> Bool
