module [
    F64,
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

F64 := {}

## Addition for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect 12.5 + 12.5 == F64.plus(12.5,12.5)
## ```
plus : F64, F64 -> F64

## Subtraction for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect 25.5 - 12.5 == F64.minus(25.5,12.5)
## ```
minus : F64, F64 -> F64

## Multiplication for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect 12.5 * 3.0 == F64.times(12.5,3.0)
## ```
times : F64, F64 -> F64

## Division for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect 36.0 / 4.0 == F64.div_by(36.0,4.0)
## ```
div_by : F64, F64 -> F64

## Truncated division for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect 37.5 // 4.0 == F64.div_trunc_by(37.5,4.0)
## ```
div_trunc_by : F64, F64 -> F64

## Exponentiation for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect 3.0 ^ 4.0 == F64.raise_to(3.0,4.0)
## ```
raise_to : F64, F64 -> F64

## Remainder for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect 37.5 % 4.0 == F64.rem_by(37.5,4.0)
## ```
rem_by : F64, F64 -> F64

## Negation for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect -12.5 == F64.negate(12.5)
## ```
negate : F64 -> F64

## Equality for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect (12.5 == 12.5) == F64.equals(12.5,12.5)
## ```
equals : F64, F64 -> Bool

## Inequality for 64-bit floating-point numbers (builtin implementation)
##
## ```
## expect (12.5 != 13.5) == F64.not_equals(12.5,13.5)
## ```
not_equals : F64, F64 -> Bool
