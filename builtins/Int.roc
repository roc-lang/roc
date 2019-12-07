interface Int
    exposes [ Int ]
    imports []

## Types

# Integer := Integer

## A 64-bit signed integer. All number literals without decimal points are #Int values.
##
## > 1
##
## > 0
##
## You can optionally put underscores in your #Int literals.
## They have no effect on the number's value, but can make large numbers easier to read.
##
## > 1_000_000
##
## See #Int.highest and #Int.lowest for the highest and
## lowest values that can be held in an #Int.
#Int : Num Integer

## Arithmetic

## Divide two integers and call #Float.floor on the result.
##
## (Use #Float.div for non-flooring division.)
##
## Return `Err DivByZero` if the second integer is zero, because division by zero is undefined in mathematics.
##
## `a // b` is shorthand for `Int.divFloor a b`.
##
## > 5 // 7
##
## > Int.divFloor 5 7
##
## > -8 // -3
##
## > Int.divFloor -8 -3
##
## This is the same as the #// operator.
#divFloor : Int, Int -> Result DivByZero Int

## Perform flooring modulo on two integers.
##
## Modulo is the same as remainder when working with positive numbers,
## but if either number is negative, then modulo works differently.
##
## Additionally, flooring modulo uses #Float.floor on the result.
##
## (Use #Float.mod for non-flooring modulo.)
##
## Return `Err DivByZero` if the second integer is zero, because division by zero is undefined in mathematics.
##
## `a %% b` is shorthand for `Int.modFloor a b`.
##
## > 5 %% 7
##
## > Int.modFloor 5 7
##
## > -8 %% -3
##
## > Int.modFloor -8 -3
#modFloor : Int, Int -> Result DivByZero Int


## Bitwise

#bitwiseXor : Int -> Int -> Int

#bitwiseAnd : Int -> Int -> Int

#bitwiseNot : Int -> Int

## Limits

## The highest number that can be stored in an #Int without overflowing its
## available memory (64 bits total) and crashing.
##
## Note that this is smaller than the positive version of #Int.lowest,
## which means if you call #Num.abs on #Int.lowest, it will crash!
#highest : Int
highest = 0x7fff_ffff_ffff_ffff

## The lowest number that can be stored in an #Int without overflowing its
## available memory (64 bits total) and crashing.
##
## Note that the positive version of this number is this is larger than
## #Int.highest, which means if you call #Num.abs on #Int.lowest,
## it will crash!
#lowest : Int
lowest = -0x8000_0000_0000_0000
