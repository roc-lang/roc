interface Set
    exposes [ Set, map, isEmpty ]
    imports []

## Check

# isEmpty : List * -> Bool

## Convert each element in the list to something new, by calling a conversion
## function on each of them. Then return a new list of the converted values.
##
## >>> Set.map {[ -1, 1, 3 ]} Num.negate
##
## >>> Set.map {[ "", "a", "bc" ]} Str.isEmpty
##
## `map` functions like this are common in Roc, and they all work similarly.
## See for example #Result.map, #List.map, and #Map.map.
map : List before, (before -> after) -> List after
