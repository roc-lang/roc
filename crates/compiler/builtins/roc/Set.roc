interface Set
    exposes [
        Set,
        empty,
        single,
        walk,
        walkUntil,
        insert,
        len,
        isEmpty,
        capacity,
        remove,
        contains,
        toList,
        fromList,
        union,
        intersection,
        difference,
        map,
        joinMap,
    ]
    imports [
        List,
        Bool.{ Bool, Eq },
        Dict.{ Dict },
        Num.{ Nat },
        Hash.{ Hash, Hasher },
    ]

## Provides a [set](https://en.wikipedia.org/wiki/Set_(abstract_data_type))
## type which stores a collection of unique values, without any ordering
Set k := Dict.Dict k {} | k has Hash & Eq
     has [
         Eq {
             isEq,
         },
         Hash {
             hash: hashSet,
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

hashSet : hasher, Set k -> hasher | k has Hash & Eq, hasher has Hasher
hashSet = \hasher, @Set inner -> Hash.hash hasher inner

## Creates a new empty `Set`.
## ```
## emptySet = Set.empty {}
## countValues = Set.len emptySet
##
## expect countValues == 0
## ```
empty : {} -> Set k | k has Hash & Eq
empty = \{} -> @Set (Dict.empty {})

## Return a dictionary with space allocated for a number of entries. This
## may provide a performance optimization if you know how many entries will be
## inserted.
withCapacity : Nat -> Set k | k has Hash & Eq
withCapacity = \cap ->
    @Set (Dict.withCapacity cap)

## Creates a new `Set` with a single value.
## ```
## singleItemSet = Set.single "Apple"
## countValues = Set.len singleItemSet
##
## expect countValues == 1
## ```
single : k -> Set k | k has Hash & Eq
single = \key ->
    Dict.single key {} |> @Set

## Insert a value into a `Set`.
## ```
## fewItemSet =
##     Set.empty {}
##     |> Set.insert "Apple"
##     |> Set.insert "Pear"
##     |> Set.insert "Banana"
##
## countValues = Set.len fewItemSet
##
## expect countValues == 3
## ```
insert : Set k, k -> Set k | k has Hash & Eq
insert = \@Set dict, key ->
    Dict.insert dict key {} |> @Set

# Inserting a duplicate key has no effect.
expect
    actual =
        empty {}
        |> insert "foo"
        |> insert "bar"
        |> insert "foo"
        |> insert "baz"

    expected =
        empty {}
        |> insert "foo"
        |> insert "bar"
        |> insert "baz"

    expected == actual

## Counts the number of values in a given `Set`.
## ```
## fewItemSet =
##     Set.empty {}
##     |> Set.insert "Apple"
##     |> Set.insert "Pear"
##     |> Set.insert "Banana"
##
## countValues = Set.len fewItemSet
##
## expect countValues == 3
## ```
len : Set * -> Nat
len = \@Set dict ->
    Dict.len dict

## Returns the max number of elements the set can hold before requiring a rehash.
## ```
## foodSet =
##     Set.empty {}
##     |> Set.insert "apple"
##
## capacityOfSet = Set.capacity foodSet
## ```
capacity : Set * -> Nat
capacity = \@Set dict ->
    Dict.capacity dict

## Check if the set is empty.
## ```
## Set.isEmpty (Set.empty {} |> Set.insert 42)
##
## Set.isEmpty (Set.empty {})
## ```
isEmpty : Set * -> Bool
isEmpty = \@Set dict ->
    Dict.isEmpty dict

# Inserting a duplicate key has no effect on length.
expect
    actual =
        empty {}
        |> insert "foo"
        |> insert "bar"
        |> insert "foo"
        |> insert "baz"
        |> len

    actual == 3

## Removes the value from the given `Set`.
## ```
## numbers =
##     Set.empty {}
##     |> Set.insert 10
##     |> Set.insert 20
##     |> Set.remove 10
##
## has10 = Set.contains numbers 10
## has20 = Set.contains numbers 20
##
## expect has10 == Bool.false
## expect has20 == Bool.true
## ```
remove : Set k, k -> Set k | k has Hash & Eq
remove = \@Set dict, key ->
    Dict.remove dict key |> @Set

## Test if a value is in the `Set`.
## ```
## Fruit : [Apple, Pear, Banana]
##
## fruit : Set Fruit
## fruit =
##     Set.single Apple
##     |> Set.insert Pear
##
## hasApple = Set.contains fruit Apple
## hasBanana = Set.contains fruit Banana
##
## expect hasApple == Bool.true
## expect hasBanana == Bool.false
## ```
contains : Set k, k -> Bool | k has Hash & Eq
contains = \@Set dict, key ->
    Dict.contains dict key

## Retrieve the values in a `Set` as a `List`.
## ```
## numbers : Set U64
## numbers = Set.fromList [1,2,3,4,5]
##
## values = [1,2,3,4,5]
##
## expect Set.toList numbers == values
## ```
toList : Set k -> List k | k has Hash & Eq
toList = \@Set dict ->
    Dict.keys dict

## Create a `Set` from a `List` of values.
## ```
## values =
##     Set.empty {}
##     |> Set.insert Banana
##     |> Set.insert Apple
##     |> Set.insert Pear
##
## expect Set.fromList [Pear, Apple, Banana] == values
## ```
fromList : List k -> Set k | k has Hash & Eq
fromList = \list ->
    initial = @Set (Dict.withCapacity (List.len list))

    List.walk list initial insert

## Combine two `Set` collection by keeping the
## [union](https://en.wikipedia.org/wiki/Union_(set_theory))
## of all the values pairs. This means that all of the values in both `Set`s
## will be combined.
## ```
## set1 = Set.single Left
## set2 = Set.single Right
##
## expect Set.union set1 set2 == Set.fromList [Left, Right]
## ```
union : Set k, Set k -> Set k | k has Hash & Eq
union = \@Set dict1, @Set dict2 ->
    Dict.insertAll dict1 dict2 |> @Set

## Combine two `Set`s by keeping the [intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory))
## of all the values pairs. This means that we keep only those values that are
## in both `Set`s.
## ```
## set1 = Set.fromList [Left, Other]
## set2 = Set.fromList [Left, Right]
##
## expect Set.intersection set1 set2 == Set.single Left
## ```
intersection : Set k, Set k -> Set k | k has Hash & Eq
intersection = \@Set dict1, @Set dict2 ->
    Dict.keepShared dict1 dict2 |> @Set

## Remove the values in the first `Set` that are also in the second `Set`
## using the [set difference](https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement)
## of the values. This means that we will be left with only those values that
## are in the first and not in the second.
## ```
## first = Set.fromList [Left, Right, Up, Down]
## second = Set.fromList [Left, Right]
##
## expect Set.difference first second == Set.fromList [Up, Down]
## ```
difference : Set k, Set k -> Set k | k has Hash & Eq
difference = \@Set dict1, @Set dict2 ->
    Dict.removeAll dict1 dict2 |> @Set

## Iterate through the values of a given `Set` and build a value.
## ```
## values = Set.fromList ["March", "April", "May"]
##
## startsWithLetterM = \month ->
##     when Str.toUtf8 month is
##         ['M', ..] -> Bool.true
##         _ -> Bool.false
##
## reduce = \state, k ->
##     if startsWithLetterM k then
##         state + 1
##     else
##         state
##
## result = Set.walk values 0 reduce
##
## expect result == 2
## ```
walk : Set k, state, (state, k -> state) -> state | k has Hash & Eq
walk = \@Set dict, state, step ->
    Dict.walk dict state (\s, k, _ -> step s k)

## Convert each value in the set to something new, by calling a conversion
## function on each of them which receives the old value. Then return a
## new set containing the converted values.
map : Set a, (a -> b) -> Set b | a has Hash & Eq, b has Hash & Eq
map = \set, transform ->
    init = withCapacity (capacity set)

    walk set init \answer, k ->
        insert answer (transform k)

## Like [Set.map], except the transformation function wraps the return value
## in a set. At the end, all the sets get joined together
## (using [Set.union]) into one set.
##
## You may know a similar function named `concatMap` in other languages.
joinMap : Set a, (a -> Set b) -> Set b | a has Hash & Eq, b has Hash & Eq
joinMap = \set, transform ->
    init = withCapacity (capacity set) # Might be a pessimization

    walk set init \answer, k ->
        union answer (transform k)

## Iterate through the values of a given `Set` and build a value, can stop
## iterating part way through the collection.
## ```
## numbers = Set.fromList [1,2,3,4,5,6,42,7,8,9,10]
##
## find42 = \state, k ->
##     if k == 42 then
##         Break FoundTheAnswer
##     else
##         Continue state
##
## result = Set.walkUntil numbers NotFound find42
##
## expect result == FoundTheAnswer
## ```
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

expect
    orderOne : Set Nat
    orderOne =
        single 1
        |> insert 2

    orderTwo : Set Nat
    orderTwo =
        single 2
        |> insert 1

    wrapperOne : Set (Set Nat)
    wrapperOne =
        single orderOne
        |> insert orderTwo

    wrapperTwo : Set (Set Nat)
    wrapperTwo =
        single orderTwo
        |> insert orderOne

    wrapperOne == wrapperTwo
