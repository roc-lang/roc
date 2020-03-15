interface List
    exposes [ List, map, fold ]
    imports []

## Types

## A list of values.
##
## >>> [ 1, 2, 3 ] # a list of ints
##
## >>> [ "a", "b", "c" ] # a list of strings
##
## >>> [ [ 1.1 ], [], [ 2.2, 3.3 ] ] # a list of lists of floats
##
## The list [ 1, "a" ] gives an error, because each element in a list must have
## the same type. If you want to put a mix of #Int and #Str values into a list, try this:
##
## ```
## mixedList : List [ IntElem Int, StrElem Str ]*
## mixedList = [ IntElem 1, IntElem 2, StrElem "a", StrElem "b" ]
## ```
##
## Lists are persistent data structures, meaning they are designed for fast copying.
##
## > Under the hood, large lists are Reduced Radix Balanced Trees.
## > Small lists are stored as flat arrays. This "small list optimization"
## > applies to lists that take up 8 machine words in memory or fewer, so
## > for example on a 64-bit system, a list of 8 #Int values will be
## > stored as a flat array instead of as an RRBT.
##
## One #List can store up to 2,147,483,648 elements (just over 2 billion). If you need to store more
## elements than that, you can split them into smaller lists and operate
## on those instead of on one large #List. This often runs faster in practice,
## even for strings much smaller than 2 gigabytes.
List elem : @List elem

## Initialize

single : elem -> List elem

## If given any number less than 1, returns #[].
repeat : elem, Int -> List elem

range : Int, Int -> List Int

## When given an #Err, returns #[], and when given #Ok, returns a list containing
## only that value.
##
## >>> List.fromResult (Ok 5)
##
## >>> List.fromResult (Err "the Err's contents get discarded")
##
## This is useful when using `List.joinMap` with a function ## that returns a #Result.
##
## > (List.joinMap [ "cat", "", "bat", "" ] Str.first) |> List.map List.fromResult
fromResult : Result elem * -> List elem

## Transform

reverse : List elem -> List elem

sort : List elem, (elem, elem -> [ Eq, Lt, Gt ]) -> List elem
sortBy : List elem, (elem -> field), (field, field -> [ Eq, Lt, Gt ]) -> List elem

## Convert each element in the list to something new, by calling a conversion
## function on each of them. Then return a new list of the converted values.
##
## > List.map [ 1, 2, 3 ] (\num -> num + 1)
##
## > List.map [ "", "a", "bc" ] Str.isEmpty
##
## `map` functions like this are common in Roc, and they all work similarly.
## See for example #Result.map, #Set.map, and #Map.map.
map : List before, (before -> after) -> List after

## This works the same way as #List.map, except it also passes the index
## of the element to the conversion function.
indexedMap : List before, (before, Int -> after) -> List after

## Add a single element to the end of a list.
##
## >>> List.append [ 1, 2, 3 ] 4
##
## >>> [ 0, 1, 2 ]
## >>>     |> List.append 3
append : List elem, elem -> List elem

## Add a single element to the beginning of a list.
##
## >>> List.prepend [ 1, 2, 3 ] 0
##
## >>> [ 2, 3, 4 ]
## >>>     |> List.prepend 1
prepend : List elem, elem -> List elem

## Put two lists together.
##
## >>> List.concat [ 1, 2, 3 ] [ 4, 5 ]
##
## >>> [ 0, 1, 2 ]
## >>>     |> List.concat [ 3, 4 ]
concat : List elem, List elem -> List elem

## Join the given lists together into one list.
##
## >>> List.join [ [ 1, 2, 3 ], [ 4, 5 ], [], [ 6, 7 ] ]
##
## >>> List.join [ [], [] ]
##
## >>> List.join []
join : List (List elem) -> List elem

joinMap : List before, (before -> List after) -> List after

## Like #List.join, but only keeps elements tagged with `Ok`. Elements
## tagged with `Err` are dropped.
##
## This can be useful after using an operation that returns a #Result
## on each element of a list, for example #List.first:
##
## >>> [ [ 1, 2, 3 ], [], [], [ 4, 5 ] ]
## >>>     |> List.map List.first
## >>>     |> List.joinOks
joinOks : List (Result elem *) -> List elem

## Iterates over the shortest of the given lists and returns a list of `Tuple`
## tags, each wrapping one of the elements in that list, along with the elements
## in the same position in # the other lists.
##
## >>> List.zip [ "a1", "b1" "c1" ] [ "a2", "b2" ] [ "a3", "b3", "c3" ]
##
## Accepts up to 8 lists.
##
## > For a generalized version that returns whatever you like, instead of a `Tup`,
## > see `zipMap`.
zip :
    List a, List b, -> List [ Tup a b ]*
    List a, List b, List c, -> List [ Tup a b c ]*
    List a, List b, List c, List d  -> List [ Tup a b c d ]*

## Like `zip` but you can specify what to do with each element.
##
## More specifically, [repeat what zip's docs say here]
##
## >>> List.zipMap [ 1, 2, 3 ] [ 0, 5, 4 ] [ 2, 1 ] \num1 num2 num3 -> num1 + num2 - num3
##
## Accepts up to 8 lists.
zipMap :
    List a, List b, (a, b) -> List c |
    List a, List b, List c, (a, b, c) -> List d |
    List a, List b, List c, List d, (a, b, c, d) -> List e


## Filter

## Run the given function on each element of a list, and return all the
## elements for which the function returned `True`.
##
## >>> List.keepIf [ 1, 2, 3, 4 ] (\num -> num > 2)
keepIf : List elem, (elem -> Bool) -> List elem

## Run the given function on each element of a list, and return all the
## elements for which the function returned `False`.
##
## >>> List.dropIf [ 1, 2, 3, 4 ] (\num -> num > 2)
dropIf : List elem, (elem -> Bool) -> List elem

## Takes the requested number of elements from the front of a list
## and returns them.
##
## >>> take 5 [ 1, 2, 3, 4, 5, 6, 7, 8 ]
##
## If there are fewer elements in the list than the requeted number,
## returns the entire list.
##
## >>> take 5 [ 1, 2 ]
take : List elem, Int -> List elem

## Drops the requested number of elements from the front of a list
## and returns the rest.
##
## >>> drop 5 [ 1, 2, 3, 4, 5, 6, 7, 8 ]
##
## If there are fewer elements in the list than the requested number to
## be dropped, returns an empty list.
##
## >>> drop 5 [ 1, 2 ]
drop : List elem, Int -> List elem

## Deconstruct

split : List elem, Int -> { before: List elem, remainder: List elem }

walk : List elem, { start : state, step : (state, elem -> state) } -> state

walkBackwards : List elem, { start : state, step : (state, elem -> state) } -> state

## Check

## Returns the length of the list - the number of elements it contains.
##
## One #List can store up to 2,147,483,648 elements (just over 2 billion), which
## is exactly equal to the highest valid #I32 value. This means the #U32 this function
## returns can always be safely converted to an #I32 without losing any data.
len : List * -> U32

isEmpty : List * -> Bool

contains : List elem, elem -> Bool

all : List elem, (elem -> Bool) -> Bool

any : List elem, (elem -> Bool) -> Bool

