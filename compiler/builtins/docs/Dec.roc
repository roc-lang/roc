interface Dec
    exposes [ add, sub, mul, div, toStr, fromStr ]
    imports [ Str ]

## Add two Dec together
add : Dec -> Dec -> Dec

## Subtract the second Dec from the first
sub : Dec -> Dec -> Dec

## Multiply two Dec together
mul : Dec -> Dec -> Dec

## Divide the first Dec by the second Dec
div : Dec -> Dec -> Dec

## Convert a Dec to a Str
## If the Dec is less than 0, this will prefix the Str with `0.`
## If the Dec has no parts after the decimal point, this will suffix the Str with `.0`
toStr : Dec -> Str

## Convert Str to a Decimal
fromStr : Str -> Result Dec [ InvalidDec ]*
