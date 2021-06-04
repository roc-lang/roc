interface Set
    exposes [ Set, empty, isEmpty, len, add, drop, map ]
    imports []

## Set

## A Set is an unordered collection of unique elements.
Set elem : [ @Set elem ]

## An empty set.
empty : Set *

## Check

isEmpty : Set * -> Bool

len : Set * -> Nat

# TODO: removed `'` from signature because parser does not support it yet
# Original signature: `add : Set 'elem, 'elem -> Set 'elem`
## Since NaN is defined to be unequal to NaN, panics if given NaN.
add : Set elem, elem -> Set elem

## Drops the given element from the set.
# TODO: removed `'` from signature because parser does not support it yet
# Original signature: `drop : Set 'elem, 'elem -> Set 'elem`
drop : Set elem, elem -> Set elem

## Convert each element in the set to something new, by calling a conversion
## function on each of them. Then return a new set of the converted values.
##
## >>> Set.map {: -1, 1, 3 :} Num.negate
##
## >>> Set.map {: "", "a", "bc" :} Str.isEmpty
##
## `map` functions like this are common in Roc, and they all work similarly.
## See for example #Result.map, #List.map, and #Map.map.
# TODO: removed `'` from signature because parser does not support it yet
# Original signature: `map : Set 'elem, ('before -> 'after) -> Set 'after`
map : Set elem, (before -> after) -> Set after
