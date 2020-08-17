interface Set
    exposes [ Set, map, isEmpty ]
    imports []


## An empty set.
empty : Set *

## Check

isEmpty : Set * -> Bool

len : Set * -> Len

add : Set 'elem, 'elem -> Set 'elem

## Drops the given element from the set.
drop : Set 'elem, 'elem -> Set 'elem

## Convert each element in the set to something new, by calling a conversion
## function on each of them. Then return a new set of the converted values.
##
## >>> Set.map {: -1, 1, 3 :} Num.negate
##
## >>> Set.map {: "", "a", "bc" :} Str.isEmpty
##
## `map` functions like this are common in Roc, and they all work similarly.
## See for example #Result.map, #List.map, and #Map.map.
map : Set 'elem, ('before -> 'after) -> Set 'after
