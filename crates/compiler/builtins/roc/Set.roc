interface Set
    exposes [
        Set,
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
    imports [List, Bool.{ Bool, Eq }, Dict.{ Dict }, Num.{ Nat }]

Set k := Dict.Dict k {} has [Eq { isEq: setEq }]

setEq = \@Set d1, @Set d2 -> d1 == d2

fromDict : Dict k {} -> Set k
fromDict = \dict -> @Set dict

toDict : Set k -> Dict k {}
toDict = \@Set dict -> dict

## An empty set.
empty : Set k
empty = fromDict Dict.empty

single : k -> Set k
single = \key ->
    @Set (Dict.single key {})

## Make sure never to insert a *NaN* to a [Set]! Because *NaN* is defined to be
## unequal to *NaN*, adding a *NaN* results in an entry that can never be
## retrieved or removed from the [Set].
insert : Set k, k -> Set k | k has Eq
insert = \@Set dict, key ->
    dict
    |> Dict.insert key {}
    |> @Set

# Inserting a duplicate key has no effect.
expect
    actual =
        Set.empty
        |> Set.insert "foo"
        |> Set.insert "bar"
        |> Set.insert "foo"
        |> Set.insert "baz"

    expected =
        Set.empty
        |> Set.insert "foo"
        |> Set.insert "bar"
        |> Set.insert "baz"

    expected == actual

len : Set k -> Nat
len = \@Set dict ->
    Dict.len dict

# Inserting a duplicate key has no effect on length.
expect
    actual =
        Set.empty
        |> Set.insert "foo"
        |> Set.insert "bar"
        |> Set.insert "foo"
        |> Set.insert "baz"
        |> Set.len

    actual == 3

## Drops the given element from the set.
remove : Set k, k -> Set k | k has Eq
remove = \@Set dict, key ->
    @Set (Dict.remove dict key)

contains : Set k, k -> Bool | k has Eq
contains = \set, key ->
    set
    |> Set.toDict
    |> Dict.contains key

toList : Set k -> List k
toList = \@Set dict ->
    Dict.keys dict

fromList : List k -> Set k | k has Eq
fromList = \list ->
    initial = @Set (Dict.withCapacity (List.len list))

    List.walk list initial \set, key -> Set.insert set key

union : Set k, Set k -> Set k | k has Eq
union = \@Set dict1, @Set dict2 ->
    @Set (Dict.insertAll dict1 dict2)

intersection : Set k, Set k -> Set k | k has Eq
intersection = \@Set dict1, @Set dict2 ->
    @Set (Dict.keepShared dict1 dict2)

difference : Set k, Set k -> Set k | k has Eq
difference = \@Set dict1, @Set dict2 ->
    @Set (Dict.removeAll dict1 dict2)

walk : Set k, state, (state, k -> state) -> state
walk = \set, state, step ->
    Dict.walk (Set.toDict set) state (\s, k, _ -> step s k)
