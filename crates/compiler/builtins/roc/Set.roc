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
        Hash.{ Hash },
    ]

# We should have this line above the next has.
# It causes the formatter to fail currently.
# | k has Hash & Eq
Set k := Dict.Dict k {}
     has [
         Eq {
             isEq,
         },
     ]

isEq : Set k, Set k -> Bool | k has Hash & Eq
isEq = \xs, ys ->
    if len xs != len ys then
        Bool.false
    else
        walkUntil xs Bool.true \_, elem ->
            if contains ys elem then
                Continue Bool.true
            else
                Break Bool.false

## An empty set.
empty : Set k | k has Hash & Eq
empty = @Set Dict.empty

single : k -> Set k | k has Hash & Eq
single = \key ->
    Dict.single key {} |> @Set

insert : Set k, k -> Set k | k has Hash & Eq
insert = \@Set dict, key ->
    Dict.insert dict key {} |> @Set

# Inserting a duplicate key has no effect.
expect
    actual =
        empty
        |> insert "foo"
        |> insert "bar"
        |> insert "foo"
        |> insert "baz"

    expected =
        empty
        |> insert "foo"
        |> insert "bar"
        |> insert "baz"

    expected == actual

len : Set k -> Nat | k has Hash & Eq
len = \@Set dict ->
    Dict.len dict

# Inserting a duplicate key has no effect on length.
expect
    actual =
        empty
        |> insert "foo"
        |> insert "bar"
        |> insert "foo"
        |> insert "baz"
        |> len

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

    List.walk list initial insert

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

walkUntil : Set k, state, (state, k -> [Continue state, Break state]) -> state | k has Hash & Eq
walkUntil = \@Set dict, state, step ->
    Dict.walkUntil dict state (\s, k, _ -> step s k)

expect
    first =
        single "Keep Me"
        |> insert "And Me"
        |> insert "Remove Me"

    second =
        single "Remove Me"
        |> insert "I do nothing..."

    expected =
        single "Keep Me"
        |> insert "And Me"

    difference first second == expected

expect
    first =
        single "Keep Me"
        |> insert "And Me"
        |> insert "Remove Me"

    second =
        single "Remove Me"
        |> insert "I do nothing..."

    expected =
        single "Keep Me"
        |> insert "And Me"

    difference first second == expected

expect
    first =
        single 1
        |> insert 2

    second =
        single 1
        |> insert 3
        |> insert 4

    expected =
        single 1
        |> insert 2
        |> insert 3
        |> insert 4

    union first second == expected

expect
    base =
        single "Remove Me"
        |> insert "Keep Me"
        |> insert "And Me"

    expected =
        single "Keep Me"
        |> insert "And Me"

    remove base "Remove Me" == expected

expect
    x =
        single 0
        |> insert 1
        |> insert 2
        |> insert 3
        |> insert 4
        |> insert 5
        |> insert 6
        |> insert 7
        |> insert 8
        |> insert 9

    x == fromList (toList x)
