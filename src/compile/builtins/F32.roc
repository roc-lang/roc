module [
    F32,
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

F32 := {}

## Addition for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect 12.5 + 12.5 == F32.plus(12.5,12.5)
## ```
plus : F32, F32 -> F32

## Subtraction for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect 25.5 - 12.5 == F32.minus(25.5,12.5)
## ```
minus : F32, F32 -> F32

## Multiplication for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect 12.5 * 3.0 == F32.times(12.5,3.0)
## ```
times : F32, F32 -> F32

## Division for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect 36.0 / 4.0 == F32.div_by(36.0,4.0)
## ```
div_by : F32, F32 -> F32

## Truncated division for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect 37.5 // 4.0 == F32.div_trunc_by(37.5,4.0)
## ```
div_trunc_by : F32, F32 -> F32

## Exponentiation for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect 3.0 ^ 4.0 == F32.raise_to(3.0,4.0)
## ```
raise_to : F32, F32 -> F32

## Remainder for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect 37.5 % 4.0 == F32.rem_by(37.5,4.0)
## ```
rem_by : F32, F32 -> F32

## Negation for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect -12.5 == F32.negate(12.5)
## ```
negate : F32 -> F32

## Equality for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect (12.5 == 12.5) == F32.equals(12.5,12.5)
## ```
equals : F32, F32 -> Bool

## Inequality for 32-bit floating-point numbers (builtin implementation)
##
## ```
## expect (12.5 != 13.5) == F32.not_equals(12.5,13.5)
## ```
not_equals : F32, F32 -> Bool
