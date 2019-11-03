interface List
    exposes [ List, ListWasEmpty, map, fold ]
    imports []

## Types

## A list of values.
##
## > [ 1, 2, 3 ] # a list of ints
##
## > [ "a", "b", "c" ] # a list of strings
##
## > [ [ 1.1 ], [], [ 2.2, 3.3 ] ] # a list of lists of floats
##
## The list [ 1, "a" ] gives an error, because each element in a list must have
## the same type. If you want to put a mix of @Int and @Str values into a list, try this:
##
## ```
## IntOrString := IntElem Int, StrElem Str
##
## mixedList : List IntOrString
## mixedList = [ IntElem 1, IntElem 2, StrElem "a", StrElem "b" ]
## ```
##
## Lists are persistent data structures, meaning they are designed for fast copying.
## Under the hood, large lists are Reduced Radix Balanced Trees. Small lists are
## stored as flat arrays. This "small list optimization" applies to lists that
## take up 8 machine words in memory or fewer, so for example on a 64-bit system,
## a list of 8 @Int values will be stored as a flat array instead of an RRBT.
Float : Num FloatingPoint

## Returned in an @Err by functions like @List.first and @List.last when they are
## passed an empty list.
ListWasEmpty := ListWasEmpty

## Returned in an @Err by @List.zip when given lists of different lengths.
DifferentLengths := DifferentLengths

## Initialize

single : elem -> List elem

## If given any number less than 1, returns @[].
repeat : elem, Int -> List elem

range : Int, Int -> List Int

## When given an @Err, returns @[], and when given @Ok, returns a list containing
## only that value.
##
## > List.fromResult (Ok 5)
##
## > List.fromResult (Err "the Err's contents get discarded")
##
## This is useful when using `List.joinMap` with a function ## that returns a @Result.
##
## > (List.joinMap [ "cat", "", "bat", "" ] Str.first) |> List.map List.fromResult
fromResult : Result elem * -> List elem

## Transform

reverse : List elem -> List elem

sort : List elem, Sorter elem -> List elem

## Convert each element in the list to something new, by calling a conversion
## function on each of them. Then return a new list of the converted values.
##
## > List.map [ 1, 2, 3 ] (\num -> num + 1)
##
## > List.map [ "a", "bc", "def" ] Str.len
##
map : List before, (before -> after) -> List after

mapWithIndex : List before, (before, Int -> after) -> List after

concat : List elem, List elem -> List elem

join : List (List elem) -> List elem

joinMap : List before, (before -> List after) -> List after

## Useful when applying an operation that returns a @Result on each element of
## a list.
##
## > (List.joinMap [ "cat", "", "bat", "" ] Str.first) |> List.map List.fromResult
joinIfOk : List before, (before -> Result * after) -> List after

## Most zip implementations silently swallow the error case where they are different lengths. We don't do that!
## If you don't think that can happen, you can always swallow it yourself by returning empty list in that case.
zip : List first, List second -> Result (List { first, second }) DifferentLengths

## Filter

keepIf : List elem, (elem -> Bool) -> List elem

dropIf : List elem, (elem -> Bool) -> List elem

## > take 5 [ 1, 2, 3, 4, 5, 6, 7, 8 ]
take : List elem, Int -> List elem

## > keep 2 4 [ 1, 2, 3, 4, 5, 6, 7, 8 ]
keep : List elem, Int, Int -> List elem

## > drop 2 4 [ 1, 2, 3, 4, 5, 6, 7, 8 ]
drop : List elem, Int, Int -> List elem

## Deconstruct

unzip : List { first, second } -> { first: List first, second: List second }

split : Int, List elem -> { before: List elem, remainder: List elem }

walk : List elem, { start : state, step : (state, elem -> state) } -> state

walkBackwards : List elem, { start : state, step : (state, elem -> state) } -> state

## Check

isEmpty : List * -> Bool

contains : List elem, elem -> Bool

all : List elem, (elem -> Bool) -> Bool

any : List elem, (elem -> Bool) -> Bool
