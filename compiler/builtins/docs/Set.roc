interface Set2
    exposes [ empty, isEmpty, len, add, drop, map ]
    imports []


## An empty set.
empty : Set *

## Check

isEmpty : Set * -> Bool

len : Set * -> Len

# TODO: removed `'` from signature because parser does not support it yet
# Original signature: `add : Set 'elem, 'elem -> Set 'elem`
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
