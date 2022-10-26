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
    imports [
        List,
        Bool.{ Bool, Eq },
        Dict.{ Dict },
        Num.{ Nat },
        Hash.{ Hasher, Hash },
    ]

Set k := Dict.Dict k {}
    # TODO: re-add type definition one #4408 is fixed
    # | k has Hash & Eq
    has [
            Eq {
                isEq,
            },
        ]

isEq : Set k, Set k -> Bool | k has Hash & Eq
isEq = \_, _ -> Bool.true

## An empty set.
empty : Set k | k has Hash & Eq
empty = @Set Dict.empty

single : k -> Set k | k has Hash & Eq
single = \key ->
    Dict.single key {} |> @Set

## Make sure never to insert a *NaN* to a [Set]! Because *NaN* is defined to be
## unequal to *NaN*, adding a *NaN* results in an entry that can never be
## retrieved or removed from the [Set].
insert : Set k, k -> Set k | k has Hash & Eq
insert = \@Set dict, key ->
    Dict.insert dict key {} |> @Set

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

len : Set k -> Nat | k has Hash & Eq
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
remove : Set k, k -> Set k | k has Hash & Eq
remove = \@Set dict, key ->
    Dict.remove dict key |> @Set

contains : Set k, k -> Bool | k has Hash & Eq
contains = \@Set dict, key ->
    Dict.contains dict key

toList : Set k -> List k | k has Hash & Eq
toList = \@Set dict ->
    Dict.keys dict

fromList : List k -> Set k | k has Hash & Eq
fromList = \list ->
    initial = @Set (Dict.withCapacity (List.len list))

    List.walk list initial \set, key -> Set.insert set key

union : Set k, Set k -> Set k | k has Hash & Eq
union = \@Set dict1, @Set dict2 ->
    Dict.insertAll dict1 dict2 |> @Set

intersection : Set k, Set k -> Set k | k has Hash & Eq
intersection = \@Set dict1, @Set dict2 ->
    Dict.keepShared dict1 dict2 |> @Set

difference : Set k, Set k -> Set k | k has Hash & Eq
difference = \@Set dict1, @Set dict2 ->
    Dict.removeAll dict1 dict2 |> @Set

walk : Set k, state, (state, k -> state) -> state | k has Hash & Eq
walk = \@Set dict, state, step ->
    Dict.walk dict state (\s, k, _ -> step s k)
