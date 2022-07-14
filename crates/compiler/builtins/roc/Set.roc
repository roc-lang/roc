interface Set
    exposes [
        empty,
        single,
        walk,
        insert,
        len,
        remove,
        contains,
        toList,
        fromList,
        union,
        intersection,
        difference,
    ]
    imports [List, Bool.{ Bool }, Dict.{ values }]

## An empty set.
empty : Set k
single : k -> Set k

## Make sure never to insert a *NaN* to a [Set]! Because *NaN* is defined to be
## unequal to *NaN*, adding a *NaN* results in an entry that can never be
## retrieved or removed from the [Set].
insert : Set k, k -> Set k
len : Set k -> Nat
len = \set ->
    set
    |> Set.toDict
    |> Dict.len

## Drops the given element from the set.
remove : Set k, k -> Set k

contains : Set k, k -> Bool
contains = \set, key ->
    set
    |> Set.toDict
    |> Dict.contains key

# toList = \set -> Dict.keys (toDict set)
toList : Set k -> List k
fromList : List k -> Set k

union : Set k, Set k -> Set k
intersection : Set k, Set k -> Set k
difference : Set k, Set k -> Set k

toDict : Set k -> Dict k {}

walk : Set k, state, (state, k -> state) -> state
walk = \set, state, step ->
    Dict.walk (Set.toDict set) state (\s, k, _ -> step s k)
