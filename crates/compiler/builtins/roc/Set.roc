module [
    Set,
    empty,
    with_capacity,
    reserve,
    release_excess_capacity,
    single,
    walk,
    walk_until,
    keep_if,
    drop_if,
    insert,
    len,
    is_empty,
    capacity,
    remove,
    contains,
    to_list,
    from_list,
    union,
    intersection,
    difference,
    map,
    join_map,
]

import List
import Bool exposing [Bool, Eq]
import Dict
import Num exposing [U64]
import Hash exposing [Hash, Hasher]
import Inspect exposing [Inspect, Inspector, InspectFormatter]

## Provides a [set](https://en.wikipedia.org/wiki/Set_(abstract_data_type))
## type which stores a collection of unique values, without any ordering
Set k := Dict.Dict k () where k implements Hash & Eq
    implements [
        Eq {
            is_eq,
        },
        Hash {
            hash: hash_set,
        },
        Inspect {
            to_inspector: to_inspector_set,
        },
    ]

is_eq : Set k, Set k -> Bool
is_eq = \xs, ys ->
    if len(xs) != len(ys) then
        Bool.false
    else
        walk_until(
            xs,
            Bool.true,
            \_, elem ->
                if contains(ys, elem) then
                    Continue(Bool.true)
                else
                    Break(Bool.false),
        )

hash_set : hasher, Set k -> hasher where hasher implements Hasher
hash_set = \hasher, @Set(inner) -> Hash.hash(hasher, inner)

to_inspector_set : Set k -> Inspector f where k implements Inspect & Hash & Eq, f implements InspectFormatter
to_inspector_set = \set ->
    Inspect.custom(
        \fmt ->
            Inspect.apply(Inspect.set(set, walk, Inspect.to_inspector), fmt),
    )

## Creates a new empty `Set`.
## ```roc
## empty_set = Set.empty()
## count_values = Set.len(empty_set)
##
## expect count_values == 0
## ```
empty : () -> Set *
empty = \() -> @Set(Dict.empty())

## Return a set with space allocated for a number of entries. This
## may provide a performance optimization if you know how many entries will be
## inserted.
with_capacity : U64 -> Set *
with_capacity = \cap ->
    @Set(Dict.with_capacity(cap))

## Enlarge the set for at least capacity additional elements
reserve : Set k, U64 -> Set k
reserve = \@Set(dict), requested ->
    @Set(Dict.reserve(dict, requested))

## Shrink the memory footprint of a set such that capacity is as small as possible.
## This function will require regenerating the metadata if the size changes.
## There will still be some overhead due to dictionary metadata always being a power of 2.
release_excess_capacity : Set k -> Set k
release_excess_capacity = \@Set(dict) ->
    @Set(Dict.release_excess_capacity(dict))

## Creates a new `Set` with a single value.
## ```roc
## single_item_set = Set.single("Apple")
## count_values = Set.len(single_item_set)
##
## expect count_values == 1
## ```
single : k -> Set k
single = \key ->
    Dict.single(key, ()) |> @Set

## Insert a value into a `Set`.
## ```roc
## few_item_set =
##     Set.empty()
##     |> Set.insert("Apple")
##     |> Set.insert("Pear")
##     |> Set.insert("Banana")
##
## count_values = Set.len(few_item_set)
##
## expect count_values == 3
## ```
insert : Set k, k -> Set k
insert = \@Set(dict), key ->
    Dict.insert(dict, key, ()) |> @Set

# Inserting a duplicate key has no effect.
expect
    actual =
        empty()
        |> insert("foo")
        |> insert("bar")
        |> insert("foo")
        |> insert("baz")

    expected =
        empty()
        |> insert("foo")
        |> insert("bar")
        |> insert("baz")

    expected == actual

## Counts the number of values in a given `Set`.
## ```roc
## few_item_set =
##     Set.empty()
##     |> Set.insert("Apple")
##     |> Set.insert("Pear")
##     |> Set.insert("Banana")
##
## count_values = Set.len(few_item_set)
##
## expect count_values == 3
## ```
len : Set * -> U64
len = \@Set(dict) ->
    Dict.len(dict)

## Returns the max number of elements the set can hold before requiring a rehash.
## ```roc
## food_set =
##     Set.empty()
##     |> Set.insert("apple")
##
## capacity_of_set = Set.capacity(food_set)
## ```
capacity : Set * -> U64
capacity = \@Set(dict) ->
    Dict.capacity(dict)

## Check if the set is empty.
## ```roc
## Set.is_empty(Set.empty() |> Set.insert(42))
##
## Set.is_empty(Set.empty())
## ```
is_empty : Set * -> Bool
is_empty = \@Set(dict) ->
    Dict.is_empty(dict)

# Inserting a duplicate key has no effect on length.
expect
    actual =
        empty()
        |> insert("foo")
        |> insert("bar")
        |> insert("foo")
        |> insert("baz")
        |> len

    actual == 3

## Removes the value from the given `Set`.
## ```roc
## numbers =
##     Set.empty()
##     |> Set.insert(10)
##     |> Set.insert(20)
##     |> Set.remove(10)
##
## has10 = Set.contains(numbers, 10)
## has20 = Set.contains(numbers, 20)
##
## expect has10 == Bool.false
## expect has20 == Bool.true
## ```
remove : Set k, k -> Set k
remove = \@Set(dict), key ->
    Dict.remove(dict, key) |> @Set

## Test if a value is in the `Set`.
## ```roc
## Fruit : [Apple, Pear, Banana]
##
## fruit : Set Fruit
## fruit =
##     Set.single(Apple)
##     |> Set.insert(Pear)
##
## has_apple = Set.contains(fruit, Apple)
## has_banana = Set.contains(fruit, Banana)
##
## expect has_apple == Bool.true
## expect has_banana == Bool.false
## ```
contains : Set k, k -> Bool
contains = \@Set(dict), key ->
    Dict.contains(dict, key)

## Retrieve the values in a `Set` as a `List`.
## ```roc
## numbers : Set U64
## numbers = Set.from_list([1,2,3,4,5])
##
## values = [1,2,3,4,5]
##
## expect Set.to_list(numbers) == values
## ```
to_list : Set k -> List k
to_list = \@Set(dict) ->
    Dict.keys(dict)

## Create a `Set` from a `List` of values.
## ```roc
## values =
##     Set.empty()
##     |> Set.insert(Banana)
##     |> Set.insert(Apple)
##     |> Set.insert(Pear)
##
## expect Set.from_list([Pear, Apple, Banana]) == values
## ```
from_list : List k -> Set k
from_list = \list ->
    list
    |> List.map(\k -> (k, ()))
    |> Dict.from_list
    |> @Set

## Combine two `Set` collection by keeping the
## [union](https://en.wikipedia.org/wiki/Union_(set_theory))
## of all the values pairs. This means that all of the values in both `Set`s
## will be combined.
## ```roc
## set1 = Set.single(Left)
## set2 = Set.single(Right)
##
## expect Set.union(set1, set2) == Set.from_list([Left, Right])
## ```
union : Set k, Set k -> Set k
union = \@Set(dict1), @Set(dict2) ->
    Dict.insert_all(dict1, dict2) |> @Set

## Combine two `Set`s by keeping the [intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory))
## of all the values pairs. This means that we keep only those values that are
## in both `Set`s.
## ```roc
## set1 = Set.from_list([Left, Other])
## set2 = Set.from_list([Left, Right])
##
## expect Set.intersection(set1, set2) == Set.single(Left)
## ```
intersection : Set k, Set k -> Set k
intersection = \@Set(dict1), @Set(dict2) ->
    Dict.keep_shared(dict1, dict2) |> @Set

## Remove the values in the first `Set` that are also in the second `Set`
## using the [set difference](https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement)
## of the values. This means that we will be left with only those values that
## are in the first and not in the second.
## ```roc
## first = Set.from_list([Left, Right, Up, Down])
## second = Set.from_list([Left, Right])
##
## expect Set.difference(first, second) == Set.from_list([Up, Down])
## ```
difference : Set k, Set k -> Set k
difference = \@Set(dict1), @Set(dict2) ->
    Dict.remove_all(dict1, dict2) |> @Set

## Iterate through the values of a given `Set` and build a value.
## ```roc
## values = Set.from_list(["March", "April", "May"])
##
## starts_with_letter_m = \month ->
##     when Str.to_utf8(month) is
##         ['M', ..] -> Bool.true
##         _ -> Bool.false
##
## reduce = \state, k ->
##     if starts_with_letter_m(k) then
##         state + 1
##     else
##         state
##
## result = Set.walk(values, 0, reduce)
##
## expect result == 2
## ```
walk : Set k, state, (state, k -> state) -> state
walk = \@Set(dict), state, step ->
    Dict.walk(dict, state, \s, k, _ -> step(s, k))

## Convert each value in the set to something new, by calling a conversion
## function on each of them which receives the old value. Then return a
## new set containing the converted values.
map : Set a, (a -> b) -> Set b
map = \set, transform ->
    init = with_capacity(capacity(set))

    walk(
        set,
        init,
        \answer, k ->
            insert(answer, transform(k)),
    )

## Like [Set.map], except the transformation function wraps the return value
## in a set. At the end, all the sets get joined together
## (using [Set.union]) into one set.
##
## You may know a similar function named `concat_map` in other languages.
join_map : Set a, (a -> Set b) -> Set b
join_map = \set, transform ->
    init = with_capacity(capacity(set)) # Might be a pessimization

    walk(
        set,
        init,
        \answer, k ->
            union(answer, transform(k)),
    )

## Iterate through the values of a given `Set` and build a value, can stop
## iterating part way through the collection.
## ```roc
## numbers = Set.from_list([1,2,3,4,5,6,42,7,8,9,10])
##
## find42 = \state, k ->
##     if k == 42 then
##         Break(FoundTheAnswer)
##     else
##         Continue(state)
##
## result = Set.walk_until(numbers, NotFound, find42)
##
## expect result == FoundTheAnswer
## ```
walk_until : Set k, state, (state, k -> [Continue state, Break state]) -> state
walk_until = \@Set(dict), state, step ->
    Dict.walk_until(dict, state, \s, k, _ -> step(s, k))

## Run the given function on each element in the `Set`, and return
## a `Set` with just the elements for which the function returned `Bool.true`.
## ```roc
## expect Set.from_list([1,2,3,4,5])
##     |> Set.keep_if(\k -> k >= 3)
##     |> Bool.is_eq(Set.from_list([3,4,5]))
## ```
keep_if : Set k, (k -> Bool) -> Set k
keep_if = \@Set(dict), predicate ->
    @Set(Dict.keep_if(dict, \(k, _v) -> predicate(k)))

## Run the given function on each element in the `Set`, and return
## a `Set` with just the elements for which the function returned `Bool.false`.
## ```roc
## expect Set.from_list [1,2,3,4,5]
##     |> Set.drop_if(\k -> k >= 3)
##     |> Bool.is_eq(Set.from_list([1,2]))
## ```
drop_if : Set k, (k -> Bool) -> Set k
drop_if = \@Set(dict), predicate ->
    @Set(Dict.drop_if(dict, \(k, _v) -> predicate(k)))

expect
    first =
        single("Keep Me")
        |> insert("And Me")
        |> insert("Remove Me")

    second =
        single("Remove Me")
        |> insert("I do nothing...")

    expected =
        single("Keep Me")
        |> insert("And Me")

    difference(first, second) == expected

expect
    first =
        single("Keep Me")
        |> insert("And Me")
        |> insert("Remove Me")

    second =
        single("Remove Me")
        |> insert("I do nothing...")

    expected =
        single("Keep Me")
        |> insert("And Me")

    difference(first, second) == expected

expect
    first =
        single(1)
        |> insert(2)

    second =
        single(1)
        |> insert(3)
        |> insert(4)

    expected =
        single(1)
        |> insert(2)
        |> insert(3)
        |> insert(4)

    union(first, second) == expected

expect
    base =
        single("Remove Me")
        |> insert("Keep Me")
        |> insert("And Me")

    expected =
        single("Keep Me")
        |> insert("And Me")

    remove(base, "Remove Me") == expected

expect
    x =
        single(0)
        |> insert(1)
        |> insert(2)
        |> insert(3)
        |> insert(4)
        |> insert(5)
        |> insert(6)
        |> insert(7)
        |> insert(8)
        |> insert(9)

    x == from_list(to_list(x))

expect
    order_one : Set U64
    order_one =
        single(1)
        |> insert(2)

    order_two : Set U64
    order_two =
        single(2)
        |> insert(1)

    wrapper_one : Set (Set U64)
    wrapper_one =
        single(order_one)
        |> insert(order_two)

    wrapper_two : Set (Set U64)
    wrapper_two =
        single(order_two)
        |> insert(order_one)

    wrapper_one == wrapper_two

expect
    Set.from_list([1, 2, 3, 4, 5])
    |> Set.keep_if(\k -> k >= 3)
    |> Bool.is_eq(Set.from_list([3, 4, 5]))

expect
    Set.from_list([1, 2, 3, 4, 5])
    |> Set.drop_if(\k -> k >= 3)
    |> Bool.is_eq(Set.from_list([1, 2]))
