interface List
    exposes [ List, map, fold ]
    imports []

## Types

## A sequential list of values.
##
## >>> [ 1, 2, 3 ] # a list of numbers
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
## The maximum size of a #List is limited by the amount of heap memory available
## to the current process. If there is not enough memory available, attempting to
## create the list could crash. (On Linux, where [overcommit](https://www.etalabs.net/overcommit.html)
## is normally enabled, not having enough memory could result in the list appearing
## to be created just fine, but then crashing later.)
##
## > The theoretical maximum length for a list created in Roc is
## > #Int.highestUlen divided by 2. Attempting to create a list bigger than that
## > in Roc code will always fail, although in practice it is likely to fail
## > at much smaller lengths due to insufficient memory being available.
##
## ## Performance notes
##
## Under the hood, a list is a record containing a `len : Ulen` field as well
## as a pointer to a flat list of bytes.
##
## This is not a [persistent data structure](https://en.wikipedia.org/wiki/Persistent_data_structure),
## so copying it is not cheap! The reason #List is designed this way is because:
##
## * Copying small lists is typically slightly faster than copying small persistent data structures. This is because, at small sizes, persistent data structures are usually thin wrappers around flat lists anyway. They don't start conferring copying advantages until crossing a certain minimum size threshold.
## Many list operations are no faster with persistent data structures. For example, even if it were a persistent data structure, #List.map, #List.fold, and #List.keepIf would all need to traverse every element in the list and build up the result from scratch.
## * Roc's compiler optimizes many list operations into in-place mutations behind the scenes, depending on how the list is being used. For example, #List.map, #List.keepIf, and #List.set can all be optimized to perform in-place mutations.
## * If possible, it is usually best for performance to use large lists in a way where the optimizer can turn them into in-place mutations. If this is not possible, a persistent data structure might be faster - but this is a rare enough scenario that it would not be good for the average Roc program's performance if this were the way #List worked by default. Instead, you can look outside Roc's standard modules for an implementation of a persistent data structure - likely built using #List under the hood!
List elem : @List elem

## Initialize

single : elem -> List elem

empty : List *

repeat : elem, Ulen -> List elem

range : Int a, Int a -> List (Int a)

## TODO I don't think we should have this after all. *Maybe* have an Ok.toList instead?
##
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

sort : List elem, Sorter elem -> List elem

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

## Iterates over the shortest of the given lists and returns a list of `Pair`
## tags, each wrapping one of the elements in that list, along with the elements
## in the same position in # the other lists.
##
## >>> List.zip [ "a1", "b1" "c1" ] [ "a2", "b2" ] [ "a3", "b3", "c3" ]
##
## Accepts up to 8 lists.
##
## > For a generalized version that returns whatever you like, instead of a `Pair`,
## > see `zipMap`.
zip :
    List a, List b, -> List [ Pair a b ]*
    List a, List b, List c, -> List [ Pair a b c ]*
    List a, List b, List c, List d  -> List [ Pair a b c d ]*

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
##
## ## Performance Notes
##
## #List.keepIf always returns a list that takes up exactly the same amount
## of memory as the original, even if its length decreases. This is becase it
## can't know in advance exactly how much space it will need, and if it guesses a
## length that's too low, it would have to re-allocate.
##
## (If you want to do an operation like this which reduces the memory footprint
## of the resulting list, you can do two passes over the lis with #List.fold - one
## to calculate the precise new size, and another to populate the new list.)
##
## If given a unique list, #List.keepIf will mutate it in place to assemble the appropriate list.
## If that happens, this function will not allocate any new memory on the heap.
## If all elements in the list end up being kept, Roc will return the original
## list unaltered.
##
keepIf : List elem, (elem -> [True, False]) -> List elem

## Run the given function on each element of a list, and return all the
## elements for which the function returned `False`.
##
## >>> List.dropIf [ 1, 2, 3, 4 ] (\num -> num > 2)
##
## ## Performance Notes
##
## #List.dropIf has the same performance characteristics as #List.keepIf.
## See its documentation for details on those characteristics!
dropIf : List elem, (elem -> [True, False]) -> List elem

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

## Access

first : List elem -> [Ok elem, ListWasEmpty]*

last : List elem -> [Ok elem, ListWasEmpty]*

get : List elem, Ulen -> [Ok elem, OutOfBounds]*

max : List (Num a) -> [Ok (Num a), ListWasEmpty]*

min : List (Num a) -> [Ok (Num a), ListWasEmpty]*

## Modify

set : List elem, Ulen, elem -> List elem

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
len : List * -> Ulen

isEmpty : List * -> Bool

contains : List elem, elem -> Bool

all : List elem, (elem -> Bool) -> Bool

any : List elem, (elem -> Bool) -> Bool
