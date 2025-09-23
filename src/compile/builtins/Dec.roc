module [
    Dec,
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

Dec := {}

## Decimal addition (builtin implementation)
##
## ```
## expect 12.5 + 12.5 == Dec.plus(12.5,12.5)
## ```
plus : Dec, Dec -> Dec

## Decimal subtraction (builtin implementation)
##
## ```
## expect 12.5 - 5.5 == Dec.minus(12.5,5.5)
## ```
minus : Dec, Dec -> Dec

## Decimal multiplication (builtin implementation)
##
## ```
## expect 3.5 * 4.0 == Dec.times(3.5,4.0)
## ```
times : Dec, Dec -> Dec

## Decimal division (builtin implementation)
##
## ```
## expect 12.0 / 3.0 == Dec.div_by(12.0,3.0)
## ```
div_by : Dec, Dec -> Dec

## Decimal truncated division (builtin implementation)
##
## ```
## expect 7.5 // 2.0 == Dec.div_trunc_by(7.5,2.0)
## ```
div_trunc_by : Dec, Dec -> Dec

## Decimal exponentiation (builtin implementation)
##
## ```
## expect 2.5 ^ 3.0 == Dec.raise_to(2.5,3.0)
## ```
raise_to : Dec, Dec -> Dec

## Decimal remainder (builtin implementation)
##
## ```
## expect 7.5 % 3.0 == Dec.rem_by(7.5,3.0)
## ```
rem_by : Dec, Dec -> Dec

## Decimal negation (builtin implementation)
##
## ```
## expect -12.5 == Dec.negate(12.5)
## ```
negate : Dec -> Dec

## Decimal equality (builtin implementation)
##
## ```
## expect (12.5 == 12.5) == Dec.equals(12.5,12.5)
## ```
equals : Dec, Dec -> Bool

## Decimal inequality (builtin implementation)
##
## ```
## expect (12.5 != 13.5) == Dec.not_equals(12.5,13.5)
## ```
not_equals : Dec, Dec -> Bool
