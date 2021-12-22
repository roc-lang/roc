interface Set
    exposes
        [
            Set,
            contains,
            difference,
            empty,
            fromList,
            insert,
            intersection,
            len,
            remove,
            single,
            toList,
            union,
            walk
        ]
    imports []

## A Set is an unordered collection of unique elements.
Set elem : [ @Set elem ]

## An empty set.
empty : Set *

## Check

isEmpty : Set * -> Bool

len : Set * -> Nat

## Modify

# TODO: removed `'` from signature because parser does not support it yet
# Original signature: `add : Set 'elem, 'elem -> Set 'elem`
## Make sure never to add a *NaN* to a [Set]! Because *NaN* is defined to be
## unequal to *NaN*, adding a *NaN* results in an entry that can never be
## retrieved or removed from the [Set].
add : Set elem, elem -> Set elem

## Drops the given element from the set.
# TODO: removed `'` from signature because parser does not support it yet
# Original signature: `drop : Set 'elem, 'elem -> Set 'elem`
drop : Set elem, elem -> Set elem

## Transform

## Convert each element in the set to something new, by calling a conversion
## function on each of them. Then return a new set of the converted values.
##
## >>> Set.map {: -1, 1, 3 :} Num.negate
##
## >>> Set.map {: "", "a", "bc" :} Str.isEmpty
##
## `map` functions like this are common in Roc, and they all work similarly.
## See for example [List.map], `Dict.map`, and [Result.map].
# TODO: removed `'` from signature because parser does not support it yet
# Original signature: `map : Set 'elem, ('before -> 'after) -> Set 'after`
map : Set elem, (before -> after) -> Set after
