api Int provides Int, Integer

## Types

Integer := Integer

## A 64-bit signed integer. All number literals without decimal points are @Int values.
##
## > 1
##
## > 0
##
## You can optionally put underscores in your @Int literals.
## They have no effect on the number's value, but can make large numbers easier to read.
##
## > 1_000_000
##
## See @Int.highestSupported and @Int.lowestSupported for the highest and
## lowest values that can be held in an @Int.
Int : Num Integer

## Arithmetic

## Divide two integers and call @Float.floor on the result.
##
## (Use @Float.div for non-flooring division.)
##
## Return `Err DivisionByZero` if the second integer is zero, because division by zero is undefined in mathematics.
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
## This is the same as the @// operator.
divFloor : Int, Int -> Result DivisionByZero Int

## Perform flooring modulo on two integers.
##
## Modulo is the same as remainder when working with positive numbers,
## but if either number is negative, then modulo works differently.
##
## Additionally, flooring modulo uses @Float.floor on the result.
##
## (Use @Float.mod for non-flooring modulo.)
##
## Return `Err DivisionByZero` if the second integer is zero, because division by zero is undefined in mathematics.
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
divFloor : Int, Int -> Result DivisionByZero Int


## Bitwise

bitwiseXor : Int -> Int -> Int

bitwiseAnd : Int -> Int -> Int

bitwiseNot : Int -> Int

## Limits

## The highest number that can be stored in an @Int without overflowing its
## available memory (64 bits total) and crashing.
##
## Note that this is smaller than the positive version of @Int.lowestSupported,
## which means if you call @Num.abs on @Int.lowestSupported, it will crash!
highestSupported : Int

## The lowest number that can be stored in an @Int without overflowing its
## available memory (64 bits total) and crashing.
##
## Note that the positive version of this number is this is larger than
## @Int.highestSupported, which means if you call @Num.abs on @Int.lowestSupported,
## it will crash!
lowestSupported : Int
