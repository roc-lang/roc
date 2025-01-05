module [
    Dict,
    empty,
    with_capacity,
    single,
    clear,
    capacity,
    reserve,
    release_excess_capacity,
    len,
    is_empty,
    get,
    contains,
    insert,
    remove,
    update,
    walk,
    walk_until,
    keep_if,
    drop_if,
    to_list,
    from_list,
    keys,
    values,
    insert_all,
    keep_shared,
    remove_all,
    map,
    join_map,
]

import Bool exposing [Bool, Eq]
import Result exposing [Result]
import List
import Str
import Num exposing [U64, F32, U32, U8]
import Hash exposing [Hasher, Hash]
import Inspect exposing [Inspect, Inspector, InspectFormatter]

## A [dictionary](https://en.wikipedia.org/wiki/Associative_array) that lets you
## associate keys with values.
##
## ## Inserting
##
## The most basic way to use a dictionary is to start with an empty one and
## then:
## 1. Call [Dict.insert] passing a key and a value, to associate that key with
## that value in the dictionary.
## 2. Later, call [Dict.get] passing the same key as before, and it will return
## the value you stored.
##
## Here's an example of a dictionary which uses a city's name as the key, and
## its population as the associated value.
## ```roc
## population_by_city =
##     Dict.empty({})
##     |> Dict.insert("London", 8_961_989)
##     |> Dict.insert("Philadelphia", 1_603_797)
##     |> Dict.insert("Shanghai", 24_870_895)
##     |> Dict.insert("Delhi", 16_787_941)
##     |> Dict.insert("Amsterdam", 872_680)
## ```
## ## Accessing keys or values
##
## We can use [Dict.keys] and [Dict.values] functions to get only the keys or
## only the values.
##
## You may notice that these lists have the same order as the original insertion
## order. This will be true if all you ever do is [Dict.insert] and [Dict.get] operations
## on the dictionary, but [Dict.remove] operations can change this order.
##
## ## Removing
##
## We can remove an element from the dictionary, like so:
## ```roc
## population_by_city
##     |> Dict.remove("Philadelphia")
##     |> Dict.keys
##     ==
##     ["London", "Amsterdam", "Shanghai", "Delhi"]
## ```
## Notice that the order has changed. Philadelphia was not only removed from the
## list, but Amsterdam - the last entry we inserted - has been moved into the
## spot where Philadelphia was previously. This is exactly what [Dict.remove]
## does. It removes an element and moves the most recent insertion into the
## vacated spot.
##
## This move is done as a performance optimization, and it lets [remove] have
## [constant time complexity](https://en.wikipedia.org/wiki/Time_complexity#Constant_time).
##
## Dict is inspired by [IndexMap](https://docs.rs/indexmap/latest/indexmap/map/struct.IndexMap.html).
## The internal implementation of a dictionary is almost identical to [ankerl::unordered_dense](https://github.com/martinus/unordered_dense).
## It has a list of keys value pairs that is ordered based on insertion.
## It uses a list of indices into the data as the backing of a hash map.
Dict k v := {
    buckets : List Bucket,
    data : List (k, v),
    max_bucket_capacity : U64,
    max_load_factor : F32,
    shifts : U8,
} where k implements Hash & Eq
    implements [
        Eq {
            is_eq,
        },
        Hash {
            hash: hash_dict,
        },
        Inspect {
            to_inspector: to_inspector_dict,
        },
    ]

is_eq : Dict k v, Dict k v -> Bool where v implements Eq
is_eq = \xs, ys ->
    if len(xs) != len(ys) then
        Bool.false
    else
        walk_until(
            xs,
            Bool.true,
            \_, k, x_val ->
                when get(ys, k) is
                    Ok(y_val) if y_val == x_val ->
                        Continue(Bool.true)

                    _ ->
                        Break(Bool.false),
        )

hash_dict : hasher, Dict k v -> hasher where v implements Hash, hasher implements Hasher
hash_dict = \hasher, dict -> Hash.hash_unordered(hasher, to_list(dict), List.walk)

to_inspector_dict : Dict k v -> Inspector f where k implements Inspect & Hash & Eq, v implements Inspect, f implements InspectFormatter
to_inspector_dict = \dict ->
    Inspect.custom(\fmt ->
        Inspect.apply(Inspect.dict(dict, walk, Inspect.to_inspector, Inspect.to_inspector), fmt))

## Return an empty dictionary.
## ```roc
## empty_dict = Dict.empty({})
## ```
empty : {} -> Dict * *
empty = \{} ->
    @Dict({
        buckets: [],
        data: [],
        max_bucket_capacity: 0,
        max_load_factor: default_max_load_factor,
        shifts: initial_shifts,
    })

## Return a dictionary with space allocated for a number of entries. This
## may provide a performance optimization if you know how many entries will be
## inserted.
with_capacity : U64 -> Dict * *
with_capacity = \requested ->
    empty({})
    |> reserve(requested)

## Enlarge the dictionary for at least capacity additional elements
reserve : Dict k v, U64 -> Dict k v
reserve = \@Dict({ buckets, data, max_bucket_capacity: original_max_bucket_capacity, max_load_factor, shifts }), requested ->
    current_size = List.len(data)
    requested_size = Num.add_wrap(current_size, requested)
    size = Num.min(requested_size, max_size)

    requested_shifts = calc_shifts_for_size(size, max_load_factor)
    if List.is_empty(buckets) || requested_shifts > shifts then
        (buckets0, max_bucket_capacity) = alloc_buckets_from_shift(requested_shifts, max_load_factor)
        buckets1 = fill_buckets_from_data(buckets0, data, requested_shifts)
        @Dict({
            buckets: buckets1,
            data: List.reserve(data, Num.sub_saturated(size, current_size)),
            max_bucket_capacity,
            max_load_factor,
            shifts: requested_shifts,
        })
    else
        @Dict({ buckets, data, max_bucket_capacity: original_max_bucket_capacity, max_load_factor, shifts })

## Shrink the memory footprint of a dictionary such that capacity is as small as possible.
## This function will require regenerating the metadata if the size changes.
## There will still be some overhead due to dictionary metadata always being a power of 2.
release_excess_capacity : Dict k v -> Dict k v
release_excess_capacity = \@Dict({ buckets, data, max_bucket_capacity: original_max_bucket_capacity, max_load_factor, shifts }) ->
    size = List.len(data)

    # NOTE: If we want, we technically could increase the load factor here to potentially minimize size more.
    min_shifts = calc_shifts_for_size(size, max_load_factor)
    if min_shifts < shifts then
        (buckets0, max_bucket_capacity) = alloc_buckets_from_shift(min_shifts, max_load_factor)
        buckets1 = fill_buckets_from_data(buckets0, data, min_shifts)
        @Dict({
            buckets: buckets1,
            data: List.release_excess_capacity(data),
            max_bucket_capacity,
            max_load_factor,
            shifts: min_shifts,
        })
    else
        @Dict({ buckets, data, max_bucket_capacity: original_max_bucket_capacity, max_load_factor, shifts })

## Returns the max number of elements the dictionary can hold before requiring a rehash.
## ```roc
## food_dict =
##     Dict.empty({})
##     |> Dict.insert("apple", "fruit")
##
## capacity_of_dict = Dict.capacity(food_dict)
## ```
capacity : Dict * * -> U64
capacity = \@Dict({ max_bucket_capacity }) ->
    max_bucket_capacity

## Returns a dictionary containing the key and value provided as input.
## ```roc
## expect
##     Dict.single("A", "B")
##     |> Bool.is_eq(Dict.empty({}) |> Dict.insert("A", "B"))
## ```
single : k, v -> Dict k v
single = \k, v ->
    insert(empty({}), k, v)

## Returns dictionary with the keys and values specified by the input [List].
## ```roc
## expect
##     Dict.single(1, "One")
##     |> Dict.insert(2, "Two")
##     |> Dict.insert(3, "Three")
##     |> Dict.insert(4, "Four")
##     |> Bool.is_eq(Dict.from_list([(1, "One"), (2, "Two"), (3, "Three"), (4, "Four")]))
## ```
##
## ## Performance Details
##
## This will build up from an empty dictionary to minimize totally memory use.
## If the list has few duplicate keys, it would be faster to allocate a dictionary
## with the same capacity of the list and walk it calling [Dict.insert]
from_list : List (k, v) -> Dict k v
from_list = \data ->
    List.walk(data, empty({}), \dict, (k, v) -> insert(dict, k, v))

## Returns the number of values in the dictionary.
## ```roc
## expect
##     Dict.empty({})
##     |> Dict.insert("One", "A Song")
##     |> Dict.insert("Two", "Candy Canes")
##     |> Dict.insert("Three", "Boughs of Holly")
##     |> Dict.len
##     |> Bool.is_eq(3)
## ```
len : Dict * * -> U64
len = \@Dict({ data }) ->
    List.len(data)

## Check if the dictionary is empty.
## ```roc
## Dict.is_empty(Dict.empty({}) |> Dict.insert("key", 42))
##
## Dict.is_empty(Dict.empty({}))
## ```
is_empty : Dict * * -> Bool
is_empty = \@Dict({ data }) ->
    List.is_empty(data)

## Clears all elements from a dictionary keeping around the allocation if it isn't huge.
## ```roc
## songs =
##        Dict.empty({})
##        |> Dict.insert("One", "A Song")
##        |> Dict.insert("Two", "Candy Canes")
##        |> Dict.insert("Three", "Boughs of Holly")
##
## clear_songs = Dict.clear(songs)
##
## expect Dict.len(clear_songs) == 0
## ```
clear : Dict k v -> Dict k v
clear = \@Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts }) ->
    @Dict({
        buckets: List.map(buckets, \_ -> empty_bucket),
        # use take_first to keep around the capacity
        data: List.take_first(data, 0),
        max_bucket_capacity,
        max_load_factor,
        shifts,
    })

## Convert each value in the dictionary to something new, by calling a conversion
## function on each of them which receives both the key and the old value. Then return a
## new dictionary containing the same keys and the converted values.
map : Dict k a, (k, a -> b) -> Dict k b
map = \dict, transform ->
    init = with_capacity(capacity(dict))

    walk(
        dict,
        init,
        \answer, k, v ->
            insert(answer, k, transform(k, v)),
    )

## Like [Dict.map], except the transformation function wraps the return value
## in a dictionary. At the end, all the dictionaries get joined together
## (using [Dict.insert_all]) into one dictionary.
##
## You may know a similar function named `concat_map` in other languages.
join_map : Dict a b, (a, b -> Dict x y) -> Dict x y
join_map = \dict, transform ->
    init = with_capacity(capacity(dict)) # Might be a pessimization

    walk(
        dict,
        init,
        \answer, k, v ->
            insert_all(answer, transform(k, v)),
    )

## Iterate through the keys and values in the dictionary and call the provided
## function with signature `state, k, v -> state` for each value, with an
## initial `state` value provided for the first call.
## ```roc
## expect
##     Dict.empty({})
##     |> Dict.insert("Apples", 12)
##     |> Dict.insert("Orange", 24)
##     |> Dict.walk(0, (\count, _, qty -> count + qty))
##     |> Bool.is_eq(36)
## ```
walk : Dict k v, state, (state, k, v -> state) -> state
walk = \@Dict({ data }), initial_state, transform ->
    List.walk(data, initial_state, \state, (k, v) -> transform(state, k, v))

## Same as [Dict.walk], except you can stop walking early.
##
## ## Performance Details
##
## Compared to [Dict.walk], this can potentially visit fewer elements (which can
## improve performance) at the cost of making each step take longer.
## However, the added cost to each step is extremely small, and can easily
## be outweighed if it results in skipping even a small number of elements.
##
## As such, it is typically better for performance to use this over [Dict.walk]
## if returning `Break` earlier than the last element is expected to be common.
## ```roc
## people =
##     Dict.empty({})
##     |> Dict.insert("Alice", 17)
##     |> Dict.insert("Bob", 18)
##     |> Dict.insert("Charlie", 19)
##
## is_adult = \_, _, age ->
##     if age >= 18 then
##         Break(Bool.true)
##     else
##         Continue(Bool.false)
##
## someone_is_an_adult = Dict.walk_until(people, Bool.false, is_adult)
##
## expect someone_is_an_adult == Bool.true
## ```
walk_until : Dict k v, state, (state, k, v -> [Continue state, Break state]) -> state
walk_until = \@Dict({ data }), initial_state, transform ->
    List.walk_until(data, initial_state, \state, (k, v) -> transform(state, k, v))

## Run the given function on each key-value pair of a dictionary, and return
## a dictionary with just the pairs for which the function returned `Bool.true`.
## ```roc
## expect Dict.empty({})
##     |> Dict.insert("Alice", 17)
##     |> Dict.insert("Bob", 18)
##     |> Dict.insert("Charlie", 19)
##     |> Dict.keep_if(\(_k, v) -> v >= 18)
##     |> Dict.len
##     |> Bool.is_eq(2)
## ```
keep_if : Dict k v, ((k, v) -> Bool) -> Dict k v
keep_if = \dict, predicate ->
    keep_if_help(dict, predicate, 0, Dict.len(dict))

keep_if_help : Dict k v, ((k, v) -> Bool), U64, U64 -> Dict k v
keep_if_help = \@Dict(dict), predicate, index, length ->
    if index < length then
        (key, value) = list_get_unsafe(dict.data, index)
        if predicate((key, value)) then
            keep_if_help(@Dict(dict), predicate, Num.add_wrap(index, 1), length)
        else
            keep_if_help(Dict.remove(@Dict(dict), key), predicate, index, Num.sub_wrap(length, 1))
    else
        @Dict(dict)

## Run the given function on each key-value pair of a dictionary, and return
## a dictionary with just the pairs for which the function returned `Bool.false`.
## ```roc
## expect Dict.empty({})
##     |> Dict.insert("Alice", 17)
##     |> Dict.insert("Bob", 18)
##     |> Dict.insert("Charlie", 19)
##     |> Dict.drop_if(\(_k, v) -> v >= 18)
##     |> Dict.len
##     |> Bool.is_eq(1)
## ```
drop_if : Dict k v, ((k, v) -> Bool) -> Dict k v
drop_if = \dict, predicate ->
    Dict.keep_if(dict, \e -> Bool.not(predicate(e)))

## Get the value for a given key. If there is a value for the specified key it
## will return [Ok value], otherwise return [Err KeyNotFound].
## ```roc
## dictionary =
##     Dict.empty({})
##     |> Dict.insert(1,s "Apple")
##     |> Dict.insert(2,s "Orange")
##
## expect Dict.get(dictionary, 1) == Ok("Apple")
## expect Dict.get(dictionary, 2000) == Err(KeyNotFound)
## ```
get : Dict k v, k -> Result v [KeyNotFound]
get = \dict, key ->
    find(dict, key)
    |> .result

## Check if the dictionary has a value for a specified key.
## ```roc
## expect
##     Dict.empty({})
##     |> Dict.insert(1234, "5678")
##     |> Dict.contains(1234)
##     |> Bool.is_eq(Bool.true)
## ```
contains : Dict k v, k -> Bool
contains = \dict, key ->
    find(dict, key)
    |> .result
    |> Result.is_ok

## Insert a value into the dictionary at a specified key.
## ```roc
## expect
##     Dict.empty({})
##     |> Dict.insert("Apples", 12)
##     |> Dict.get("Apples")
##     |> Bool.is_eq(Ok(12))
## ```
insert : Dict k v, k, v -> Dict k v
insert = \dict, key, value ->
    @Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts }) =
        if len(dict) < capacity(dict) then
            dict
        else
            increase_size(dict)

    hash = hash_key(key)
    dist_and_fingerprint = dist_and_fingerprint_from_hash(hash)
    bucket_index = bucket_index_from_hash(hash, shifts)

    # TODO: put these args in a record for organization?
    insert_helper(buckets, data, bucket_index, dist_and_fingerprint, key, value, max_bucket_capacity, max_load_factor, shifts)

insert_helper : List Bucket, List (k, v), U64, U32, k, v, U64, F32, U8 -> Dict k v
insert_helper = \buckets0, data0, bucket_index0, dist_and_fingerprint0, key, value, max_bucket_capacity, max_load_factor, shifts ->
    loaded = list_get_unsafe(buckets0, bucket_index0)
    if dist_and_fingerprint0 == loaded.dist_and_fingerprint then
        (found_key, _) = list_get_unsafe(data0, Num.to_u64(loaded.data_index))
        if found_key == key then
            data1 = List.set(data0, Num.to_u64(loaded.data_index), (key, value))
            @Dict({ buckets: buckets0, data: data1, max_bucket_capacity, max_load_factor, shifts })
        else
            bucket_index1 = next_bucket_index(bucket_index0, List.len(buckets0))
            dist_and_fingerprint1 = increment_dist(dist_and_fingerprint0)
            insert_helper(buckets0, data0, bucket_index1, dist_and_fingerprint1, key, value, max_bucket_capacity, max_load_factor, shifts)
    else if dist_and_fingerprint0 > loaded.dist_and_fingerprint then
        data1 = List.append(data0, (key, value))
        data_index = List.len(data1) |> Num.sub_wrap(1)
        buckets1 = place_and_shift_up(buckets0, { dist_and_fingerprint: dist_and_fingerprint0, data_index: Num.to_u32(data_index) }, bucket_index0)
        @Dict({ buckets: buckets1, data: data1, max_bucket_capacity, max_load_factor, shifts })
    else
        bucket_index1 = next_bucket_index(bucket_index0, List.len(buckets0))
        dist_and_fingerprint1 = increment_dist(dist_and_fingerprint0)
        insert_helper(buckets0, data0, bucket_index1, dist_and_fingerprint1, key, value, max_bucket_capacity, max_load_factor, shifts)

## Remove a value from the dictionary for a specified key.
## ```roc
## expect
##     Dict.empty({})
##     |> Dict.insert("Some", "Value")
##     |> Dict.remove("Some")
##     |> Dict.len
##     |> Bool.is_eq(0)
## ```
remove : Dict k v, k -> Dict k v
remove = \@Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts }), key ->
    if !(List.is_empty(data)) then
        (bucket_index0, dist_and_fingerprint0) = next_while_less(buckets, key, shifts)
        (bucket_index1, dist_and_fingerprint1) = remove_helper(buckets, bucket_index0, dist_and_fingerprint0, data, key)

        bucket = list_get_unsafe(buckets, bucket_index1)
        if dist_and_fingerprint1 != bucket.dist_and_fingerprint then
            @Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts })
        else
            remove_bucket(@Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts }), bucket_index1)
    else
        @Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts })

remove_helper : List Bucket, U64, U32, List (k, *), k -> (U64, U32) where k implements Eq
remove_helper = \buckets, bucket_index, dist_and_fingerprint, data, key ->
    bucket = list_get_unsafe(buckets, bucket_index)
    if dist_and_fingerprint == bucket.dist_and_fingerprint then
        (found_key, _) = list_get_unsafe(data, Num.to_u64(bucket.data_index))
        if found_key == key then
            (bucket_index, dist_and_fingerprint)
        else
            remove_helper(buckets, next_bucket_index(bucket_index, List.len(buckets)), increment_dist(dist_and_fingerprint), data, key)
    else
        (bucket_index, dist_and_fingerprint)

## Insert or remove a value for a specified key. This function enables a
## performance optimization for the use case of providing a default when a value
## is missing. This is more efficient than doing both a `Dict.get` and then a
## `Dict.insert` call, and supports being piped.
## ```roc
## alter_value : Result Bool [Missing] -> Result Bool [Missing]
## alter_value = \possible_value ->
##     when possible_value is
##         Err Missing -> Ok(Bool.false)
##         Ok value -> if value then Err(Missing) else Ok(Bool.true)
##
## expect Dict.update(Dict.empty({}), "a", alter_value) == Dict.single("a", Bool.false)
## expect Dict.update(Dict.single("a", Bool.false), "a", alter_value) == Dict.single("a", Bool.true)
## expect Dict.update(Dict.single("a", Bool.true), "a", alter_value) == Dict.empty({})
## ```
update : Dict k v, k, (Result v [Missing] -> Result v [Missing]) -> Dict k v
update = \@Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts }), key, alter ->
    { bucket_index, result } = find(@Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts }), key)
    when result is
        Ok(value) ->
            when alter(Ok(value)) is
                Ok(new_value) ->
                    bucket = list_get_unsafe(buckets, bucket_index)
                    new_data = List.set(data, Num.to_u64(bucket.data_index), (key, new_value))
                    @Dict({ buckets, data: new_data, max_bucket_capacity, max_load_factor, shifts })

                Err(Missing) ->
                    remove_bucket(@Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts }), bucket_index)

        Err(KeyNotFound) ->
            when alter(Err(Missing)) is
                Ok(new_value) ->
                    if List.len(data) >= max_bucket_capacity then
                        # Need to reallocate let regular insert handle that.
                        insert(@Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts }), key, new_value)
                    else
                        # Can skip work by jumping staight to the found bucket.
                        # That will be the location we want to insert in.
                        hash = hash_key(key)
                        base_dist_and_fingerprint = dist_and_fingerprint_from_hash(hash)
                        base_bucket_index = bucket_index_from_hash(hash, shifts)

                        # Due to the unrolling of loops in find along with loop optimizations,
                        # The bucket_index is not guaranteed to be correct here.
                        # It is only correct if we have traversed past the number of find unrolls.
                        dist = circular_dist(base_bucket_index, bucket_index, List.len(buckets))
                        if dist <= find_manual_unrolls then
                            insert_helper(buckets, data, base_bucket_index, base_dist_and_fingerprint, key, new_value, max_bucket_capacity, max_load_factor, shifts)
                        else
                            dist_and_fingerprint = increment_dist_n(base_dist_and_fingerprint, Num.to_u32(dist))
                            insert_helper(buckets, data, bucket_index, dist_and_fingerprint, key, new_value, max_bucket_capacity, max_load_factor, shifts)

                Err(Missing) ->
                    @Dict({ buckets, data, max_bucket_capacity, max_load_factor, shifts })

circular_dist = \start, end, size ->
    correction =
        if start > end then
            size
        else
            0
    end
    |> Num.sub_wrap(start)
    |> Num.add_wrap(correction)

## Returns the keys and values of a dictionary as a [List].
## This requires allocating a temporary list, prefer using [Dict.to_list] or [Dict.walk] instead.
## ```roc
## expect
##     Dict.single(1, "One")
##     |> Dict.insert(2, "Two")
##     |> Dict.insert(3, "Three")
##     |> Dict.insert(4, "Four")
##     |> Dict.to_list
##     |> Bool.is_eq([(1, "One"), (2, "Two"), (3, "Three"), (4, "Four")])
## ```
to_list : Dict k v -> List (k, v)
to_list = \@Dict({ data }) ->
    data

## Returns the keys of a dictionary as a [List].
## This requires allocating a temporary [List], prefer using [Dict.to_list] or [Dict.walk] instead.
## ```roc
## expect
##     Dict.single(1, "One")
##     |> Dict.insert(2, "Two")
##     |> Dict.insert(3, "Three")
##     |> Dict.insert(4, "Four")
##     |> Dict.keys
##     |> Bool.is_eq([1,2,3,4])
## ```
keys : Dict k v -> List k
keys = \@Dict({ data }) ->
    List.map(data, \(k, _) -> k)

## Returns the values of a dictionary as a [List].
## This requires allocating a temporary [List], prefer using [Dict.to_list] or [Dict.walk] instead.
## ```roc
## expect
##     Dict.single(1, "One")
##     |> Dict.insert(2, "Two")
##     |> Dict.insert(3, "Three")
##     |> Dict.insert(4, "Four")
##     |> Dict.values
##     |> Bool.is_eq(["One","Two","Three","Four"])
## ```
values : Dict k v -> List v
values = \@Dict({ data }) ->
    List.map(data, \(_, v) -> v)

## Combine two dictionaries by keeping the [union](https://en.wikipedia.org/wiki/Union_(set_theory))
## of all the key-value pairs. This means that all the key-value pairs in
## both dictionaries will be combined. Note that where there are pairs
## with the same key, the value contained in the second input will be
## retained, and the value in the first input will be removed.
## ```roc
## first =
##     Dict.single(1, "Not Me")
##     |> Dict.insert(2, "And Me")
##
## second =
##     Dict.single(1, "Keep Me")
##     |> Dict.insert(3, "Me Too")
##     |> Dict.insert(4, "And Also Me")
##
## expected =
##     Dict.single(1, "Keep Me")
##     |> Dict.insert(2, "And Me")
##     |> Dict.insert(3, "Me Too")
##     |> Dict.insert(4, "And Also Me")
##
## expect
##     Dict.insert_all(first, second) == expected
## ```
insert_all : Dict k v, Dict k v -> Dict k v
insert_all = \xs, ys ->
    if len(ys) > len(xs) then
        insert_all(ys, xs)
    else
        walk(ys, xs, insert)

## Combine two dictionaries by keeping the [intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory))
## of all the key-value pairs. This means that we keep only those pairs
## that are in both dictionaries. Both the key and value must match to be kept.
## ```roc
## first =
##     Dict.single(1, "Keep Me")
##     |> Dict.insert(2, "And Me")
##     |> Dict.insert(3, "Not this one")
##
## second =
##     Dict.single(1, "Keep Me")
##     |> Dict.insert(2, "And Me")
##     |> Dict.insert(3, "This has a different value")
##     |> Dict.insert(4, "Or Me")
##
## expected =
##     Dict.single(1, "Keep Me")
##     |> Dict.insert(2, "And Me")
##
## expect Dict.keep_shared(first, second) == expected
## ```
keep_shared : Dict k v, Dict k v -> Dict k v where v implements Eq
keep_shared = \xs0, ys0 ->
    (xs1, ys1) =
        if len(ys0) < len(xs0) then
            (ys0, xs0)
        else
            (xs0, ys0)

    walk(
        xs1,
        with_capacity(len(xs1)),
        \state, k, v ->
            when get(ys1, k) is
                Ok(yv) if v == yv ->
                    insert(state, k, v)

                _ ->
                    state,
    )

## Remove the key-value pairs in the first input that are also in the second
## using the [set difference](https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement)
## of the values. This means that we will be left with only those pairs that
## are in the first dictionary and whose keys are not in the second.
## ```roc
## first =
##     Dict.single(1, "Keep Me")
##     |> Dict.insert(2, "And Me")
##     |> Dict.insert(3, "Remove Me")
##
## second =
##     Dict.single(3, "Remove Me")
##     |> Dict.insert(4, "I do nothing...")
##
## expected =
##     Dict.single(1, "Keep Me")
##     |> Dict.insert(2, "And Me")
##
## expect Dict.remove_all(first, second) == expected
## ```
remove_all : Dict k v, Dict k v -> Dict k v
remove_all = \xs, ys ->
    walk(ys, xs, \state, k, _ -> remove(state, k))

# Below here is a list of generic helpers and internal data types for Dict
Bucket : {
    dist_and_fingerprint : U32, # upper 3 byte: distance to original bucket. lower byte: fingerprint from hash
    data_index : U32, # index into the data list.
}

empty_bucket = { dist_and_fingerprint: 0, data_index: 0 }
dist_inc = Num.shift_left_by(1u32, 8) # skip 1 byte fingerprint
fingerprint_mask = Num.sub_wrap(dist_inc, 1) # mask for 1 byte of fingerprint
default_max_load_factor = 0.8
initial_shifts = Num.sub_wrap(64, 3) # 2^(64-shifts) number of buckets
max_size = Num.shift_left_by(1u64, 32)
max_bucket_count = max_size

increment_dist = \dist_and_fingerprint ->
    Num.add_wrap(dist_and_fingerprint, dist_inc)

increment_dist_n = \dist_and_fingerprint, n ->
    Num.add_wrap(dist_and_fingerprint, Num.mul_wrap(n, dist_inc))

decrement_dist = \dist_and_fingerprint ->
    Num.sub_wrap(dist_and_fingerprint, dist_inc)

find : Dict k v, k -> { bucket_index : U64, result : Result v [KeyNotFound] }
find = \@Dict({ buckets, data, shifts }), key ->
    hash = hash_key(key)
    dist_and_fingerprint = dist_and_fingerprint_from_hash(hash)
    bucket_index = bucket_index_from_hash(hash, shifts)

    if !(List.is_empty(data)) then
        # TODO: this is true in the C++ code, confirm it in Roc as well.
        # unrolled loop. *Always* check a few directly, then enter the loop. This is faster.
        find_first_unroll(buckets, bucket_index, dist_and_fingerprint, data, key)
    else
        { bucket_index, result: Err(KeyNotFound) }

find_manual_unrolls = 2

find_first_unroll : List Bucket, U64, U32, List (k, v), k -> { bucket_index : U64, result : Result v [KeyNotFound] } where k implements Eq
find_first_unroll = \buckets, bucket_index, dist_and_fingerprint, data, key ->
    # TODO: once we have short circuit evaluation, use it here and other similar locations in this file.
    # Avoid the nested if with else block inconvenience.
    bucket = list_get_unsafe(buckets, bucket_index)
    if dist_and_fingerprint == bucket.dist_and_fingerprint then
        (found_key, value) = list_get_unsafe(data, Num.to_u64(bucket.data_index))
        if found_key == key then
            { bucket_index, result: Ok(value) }
        else
            find_second_unroll(buckets, next_bucket_index(bucket_index, List.len(buckets)), increment_dist(dist_and_fingerprint), data, key)
    else
        find_second_unroll(buckets, next_bucket_index(bucket_index, List.len(buckets)), increment_dist(dist_and_fingerprint), data, key)

find_second_unroll : List Bucket, U64, U32, List (k, v), k -> { bucket_index : U64, result : Result v [KeyNotFound] } where k implements Eq
find_second_unroll = \buckets, bucket_index, dist_and_fingerprint, data, key ->
    bucket = list_get_unsafe(buckets, bucket_index)
    if dist_and_fingerprint == bucket.dist_and_fingerprint then
        (found_key, value) = list_get_unsafe(data, Num.to_u64(bucket.data_index))
        if found_key == key then
            { bucket_index, result: Ok(value) }
        else
            find_helper(buckets, next_bucket_index(bucket_index, List.len(buckets)), increment_dist(dist_and_fingerprint), data, key)
    else
        find_helper(buckets, next_bucket_index(bucket_index, List.len(buckets)), increment_dist(dist_and_fingerprint), data, key)

find_helper : List Bucket, U64, U32, List (k, v), k -> { bucket_index : U64, result : Result v [KeyNotFound] } where k implements Eq
find_helper = \buckets, bucket_index, dist_and_fingerprint, data, key ->
    bucket = list_get_unsafe(buckets, bucket_index)
    if dist_and_fingerprint == bucket.dist_and_fingerprint then
        (found_key, value) = list_get_unsafe(data, Num.to_u64(bucket.data_index))
        if found_key == key then
            { bucket_index, result: Ok(value) }
        else
            find_helper(buckets, next_bucket_index(bucket_index, List.len(buckets)), increment_dist(dist_and_fingerprint), data, key)
    else if dist_and_fingerprint > bucket.dist_and_fingerprint then
        { bucket_index, result: Err(KeyNotFound) }
    else
        find_helper(buckets, next_bucket_index(bucket_index, List.len(buckets)), increment_dist(dist_and_fingerprint), data, key)

remove_bucket : Dict k v, U64 -> Dict k v
remove_bucket = \@Dict({ buckets: buckets0, data: data0, max_bucket_capacity, max_load_factor, shifts }), bucket_index0 ->
    data_index_to_remove = list_get_unsafe(buckets0, bucket_index0) |> .data_index
    data_index_to_remove_u64 = Num.to_u64(data_index_to_remove)

    (buckets1, bucket_index1) = remove_bucket_helper(buckets0, bucket_index0)
    buckets2 = List.set(buckets1, bucket_index1, empty_bucket)

    last_data_index = List.len(data0) |> Num.sub_wrap(1)
    if data_index_to_remove_u64 != last_data_index then
        # Swap removed item to the end
        data1 = List.swap(data0, data_index_to_remove_u64, last_data_index)
        (key, _) = list_get_unsafe(data1, data_index_to_remove_u64)

        # Update the data index of the new value.
        hash = hash_key(key)
        bucket_index2 = bucket_index_from_hash(hash, shifts)

        bucket_index3 = scan_for_index(buckets2, bucket_index2, Num.to_u32(last_data_index))
        swap_bucket = list_get_unsafe(buckets2, bucket_index3)

        @Dict({
            buckets: List.set(buckets2, bucket_index3, { swap_bucket & data_index: data_index_to_remove }),
            data: List.drop_last(data1, 1),
            max_bucket_capacity,
            max_load_factor,
            shifts,
        })
    else
        @Dict({
            buckets: buckets2,
            data: List.drop_last(data0, 1),
            max_bucket_capacity,
            max_load_factor,
            shifts,
        })

scan_for_index : List Bucket, U64, U32 -> U64
scan_for_index = \buckets, bucket_index, data_index ->
    bucket = list_get_unsafe(buckets, bucket_index)
    if bucket.data_index != data_index then
        scan_for_index(buckets, next_bucket_index(bucket_index, List.len(buckets)), data_index)
    else
        bucket_index

remove_bucket_helper : List Bucket, U64 -> (List Bucket, U64)
remove_bucket_helper = \buckets, bucket_index ->
    next_index = next_bucket_index(bucket_index, List.len(buckets))
    next_bucket = list_get_unsafe(buckets, next_index)
    # shift down until either empty or an element with correct spot is found
    if next_bucket.dist_and_fingerprint >= Num.mul_wrap(dist_inc, 2) then
        List.set(buckets, bucket_index, { next_bucket & dist_and_fingerprint: decrement_dist(next_bucket.dist_and_fingerprint) })
        |> remove_bucket_helper(next_index)
    else
        (buckets, bucket_index)

increase_size : Dict k v -> Dict k v
increase_size = \@Dict({ data, max_bucket_capacity, max_load_factor, shifts }) ->
    if max_bucket_capacity != max_bucket_count then
        new_shifts = shifts |> Num.sub_wrap(1)
        (buckets0, new_max_bucket_capacity) = alloc_buckets_from_shift(new_shifts, max_load_factor)
        buckets1 = fill_buckets_from_data(buckets0, data, new_shifts)
        @Dict({
            buckets: buckets1,
            data,
            max_bucket_capacity: new_max_bucket_capacity,
            max_load_factor,
            shifts: new_shifts,
        })
    else
        crash("Dict hit limit of $(Num.to_str(max_bucket_count)) elements. Unable to grow more.")

alloc_buckets_from_shift : U8, F32 -> (List Bucket, U64)
alloc_buckets_from_shift = \shifts, max_load_factor ->
    bucket_count = calc_num_buckets(shifts)
    if bucket_count == max_bucket_count then
        # reached the maximum, make sure we can use each bucket
        (List.repeat(empty_bucket, max_bucket_count), max_bucket_count)
    else
        max_bucket_capacity =
            bucket_count
            |> Num.to_f32
            |> Num.mul(max_load_factor)
            |> Num.floor

        (List.repeat(empty_bucket, bucket_count), max_bucket_capacity)

calc_shifts_for_size : U64, F32 -> U8
calc_shifts_for_size = \size, max_load_factor ->
    calc_shifts_for_size_helper(initial_shifts, size, max_load_factor)

calc_shifts_for_size_helper = \shifts, size, max_load_factor ->
    max_bucket_capacity =
        shifts
        |> calc_num_buckets
        |> Num.to_f32
        |> Num.mul(max_load_factor)
        |> Num.floor
    if shifts > 0 && max_bucket_capacity < size then
        calc_shifts_for_size_helper(Num.sub_wrap(shifts, 1), size, max_load_factor)
    else
        shifts

calc_num_buckets = \shifts ->
    Num.min(Num.shift_left_by(1, Num.sub_wrap(64, shifts)), max_bucket_count)

fill_buckets_from_data = \buckets0, data, shifts ->
    List.walk_with_index(
        data,
        buckets0,
        \buckets1, (key, _), data_index ->
            (bucket_index, dist_and_fingerprint) = next_while_less(buckets1, key, shifts)
            place_and_shift_up(buckets1, { dist_and_fingerprint, data_index: Num.to_u32(data_index) }, bucket_index),
    )

next_while_less : List Bucket, k, U8 -> (U64, U32) where k implements Hash & Eq
next_while_less = \buckets, key, shifts ->
    hash = hash_key(key)
    dist_and_fingerprint = dist_and_fingerprint_from_hash(hash)
    bucket_index = bucket_index_from_hash(hash, shifts)

    next_while_less_helper(buckets, bucket_index, dist_and_fingerprint)

next_while_less_helper = \buckets, bucket_index, dist_and_fingerprint ->
    loaded = list_get_unsafe(buckets, bucket_index)
    if dist_and_fingerprint < loaded.dist_and_fingerprint then
        next_while_less_helper(buckets, next_bucket_index(bucket_index, List.len(buckets)), increment_dist(dist_and_fingerprint))
    else
        (bucket_index, dist_and_fingerprint)

place_and_shift_up = \buckets0, bucket, bucket_index ->
    loaded = list_get_unsafe(buckets0, bucket_index)
    if loaded.dist_and_fingerprint != 0 then
        buckets1 = List.set(buckets0, bucket_index, bucket)
        place_and_shift_up(
            buckets1,
            { loaded & dist_and_fingerprint: increment_dist(loaded.dist_and_fingerprint) },
            next_bucket_index(bucket_index, List.len(buckets1)),
        )
    else
        List.set(buckets0, bucket_index, bucket)

next_bucket_index = \bucket_index, max_buckets ->
    # I just ported this impl directly.
    # I am a bit confused why it is using an if over a mask.
    # Maybe compilers are smart enough to optimize this well.
    # Maybe the unlikely annotation is super important
    if Num.add_wrap(bucket_index, 1) != max_buckets then
        Num.add_wrap(bucket_index, 1)
    else
        0

hash_key = \key ->
    create_low_level_hasher(PseudoRandSeed)
    |> Hash.hash(key)
    |> complete

dist_and_fingerprint_from_hash : U64 -> U32
dist_and_fingerprint_from_hash = \hash ->
    hash
    |> Num.to_u32
    |> Num.bitwise_and(fingerprint_mask)
    |> Num.bitwise_or(dist_inc)

bucket_index_from_hash : U64, U8 -> U64
bucket_index_from_hash = \hash, shifts ->
    Num.shift_right_zf_by(hash, shifts)

expect
    val =
        empty({})
        |> insert("foo", "bar")
        |> get("foo")

    val == Ok("bar")

expect
    dict1 =
        empty({})
        |> insert(1, "bar")
        |> insert(2, "baz")

    dict2 =
        empty({})
        |> insert(2, "baz")
        |> insert(1, "bar")

    dict1 == dict2

expect
    dict1 =
        empty({})
        |> insert(1, "bar")
        |> insert(2, "baz")

    dict2 =
        empty({})
        |> insert(1, "bar")
        |> insert(2, "baz!")

    dict1 != dict2

expect
    inner1 =
        empty({})
        |> insert(1, "bar")
        |> insert(2, "baz")

    inner2 =
        empty({})
        |> insert(2, "baz")
        |> insert(1, "bar")

    outer =
        empty({})
        |> insert(inner1, "wrong")
        |> insert(inner2, "right")

    get(outer, inner1) == Ok("right")

expect
    inner1 =
        empty({})
        |> insert(1, "bar")
        |> insert(2, "baz")

    inner2 =
        empty({})
        |> insert(2, "baz")
        |> insert(1, "bar")

    outer1 =
        empty({})
        |> insert(inner1, "val")

    outer2 =
        empty({})
        |> insert(inner2, "val")

    outer1 == outer2

expect
    val =
        empty({})
        |> insert("foo", "bar")
        |> insert("foo", "baz")
        |> get("foo")

    val == Ok("baz")

expect
    val =
        empty({})
        |> insert("foo", "bar")
        |> get("bar")

    val == Err(KeyNotFound)

expect
    empty({})
    |> insert("foo", {})
    |> contains("foo")

expect
    dict =
        empty({})
        |> insert("foo", {})
        |> insert("bar", {})
        |> insert("baz", {})

    contains(dict, "baz") && !(contains(dict, "other"))

expect
    dict =
        from_list([(1u8, 1u8), (2, 2), (3, 3)])
        |> remove(1)
        |> remove(3)

    keys(dict) == [2]

expect
    list =
        from_list([(1u8, 1u8), (2u8, 2u8), (3, 3)])
        |> remove(1)
        |> insert(0, 0)
        |> remove(3)
        |> keys

    list == [0, 2]

# Reach capacity, no rehash.
expect
    val =
        empty({})
        |> insert("a", 0)
        |> insert("b", 1)
        |> insert("c", 2)
        |> insert("d", 3)
        |> insert("e", 4)
        |> insert("f", 5)
        |> insert("g", 6)
        |> insert("h", 7)
        |> insert("i", 8)
        |> insert("j", 9)
        |> insert("k", 10)
        |> insert("l", 11)
        |> capacity

    val == 12

# Reach capacity, all elements still exist
expect
    dict =
        empty({})
        |> insert("a", 0)
        |> insert("b", 1)
        |> insert("c", 2)
        |> insert("d", 3)
        |> insert("e", 4)
        |> insert("f", 5)
        |> insert("g", 6)
        |> insert("h", 7)
        |> insert("i", 8)
        |> insert("j", 9)
        |> insert("k", 10)
        |> insert("l", 11)

    (get(dict, "a") == Ok(0))
    && (get(dict, "b") == Ok(1))
    && (get(dict, "c") == Ok(2))
    && (get(dict, "d") == Ok(3))
    && (get(dict, "e") == Ok(4))
    && (get(dict, "f") == Ok(5))
    && (get(dict, "g") == Ok(6))
    && (get(dict, "h") == Ok(7))
    && (get(dict, "i") == Ok(8))
    && (get(dict, "j") == Ok(9))
    && (get(dict, "k") == Ok(10))
    && (get(dict, "l") == Ok(11))

# Force rehash.
expect
    val =
        empty({})
        |> insert("a", 0)
        |> insert("b", 1)
        |> insert("c", 2)
        |> insert("d", 3)
        |> insert("e", 4)
        |> insert("f", 5)
        |> insert("g", 6)
        |> insert("h", 7)
        |> insert("i", 8)
        |> insert("j", 9)
        |> insert("k", 10)
        |> insert("l", 11)
        |> insert("m", 12)
        |> capacity

    val == 25

# Force rehash, all elements still exist
expect
    dict =
        empty({})
        |> insert("a", 0)
        |> insert("b", 1)
        |> insert("c", 2)
        |> insert("d", 3)
        |> insert("e", 4)
        |> insert("f", 5)
        |> insert("g", 6)
        |> insert("h", 7)
        |> insert("i", 8)
        |> insert("j", 9)
        |> insert("k", 10)
        |> insert("l", 11)
        |> insert("m", 12)

    (get(dict, "a") == Ok(0))
    && (get(dict, "b") == Ok(1))
    && (get(dict, "c") == Ok(2))
    && (get(dict, "d") == Ok(3))
    && (get(dict, "e") == Ok(4))
    && (get(dict, "f") == Ok(5))
    && (get(dict, "g") == Ok(6))
    && (get(dict, "h") == Ok(7))
    && (get(dict, "i") == Ok(8))
    && (get(dict, "j") == Ok(9))
    && (get(dict, "k") == Ok(10))
    && (get(dict, "l") == Ok(11))
    && (get(dict, "m") == Ok(12))

expect
    empty({})
    |> insert("Some", "Value")
    |> remove("Some")
    |> len
    |> Bool.is_eq(0)

# All BadKey's hash to the same location.
# This is needed to test some robinhood logic.
BadKey := U64 implements [
        Eq,
        Hash {
            hash: hash_bad_key,
        },
    ]

hash_bad_key : hasher, BadKey -> hasher where hasher implements Hasher
hash_bad_key = \hasher, _ -> Hash.hash(hasher, 0)

expect
    bad_keys = [
        @BadKey(0),
        @BadKey(1),
        @BadKey(2),
        @BadKey(3),
        @BadKey(4),
        @BadKey(5),
        @BadKey(6),
        @BadKey(5),
        @BadKey(4),
        @BadKey(3),
        @BadKey(3),
        @BadKey(3),
        @BadKey(10),
    ]

    dict =
        List.walk(
            bad_keys,
            Dict.empty({}),
            \acc, k ->
                Dict.update(
                    acc,
                    k,
                    \val ->
                        when val is
                            Ok(p) -> Ok(Num.add_wrap(p, 1))
                            Err(Missing) -> Ok(0),
                ),
        )

    all_inserted_correctly =
        List.walk(
            bad_keys,
            Bool.true,
            \acc, k ->
                acc && Dict.contains(dict, k),
        )

    all_inserted_correctly

# Note, there are a number of places we should probably use set and replace unsafe.
# unsafe primitive that does not perform a bounds check
list_get_unsafe : List a, U64 -> a

# We have decided not to expose the standard roc hashing algorithm.
# This is to avoid external dependence and the need for versioning.
# The current implementation is a form of [Wyhash final4](https://github.com/wangyi-fudan/wyhash/blob/77e50f267fbc7b8e2d09f2d455219adb70ad4749/wyhash.h).
# It is 64bit and little endian specific currently.
# TODO: wyhash is slow for large keys, use something like cityhash if the keys are too long.
# TODO: Add a builtin to distinguish big endian systems and change loading orders.
# TODO: Switch out Wymum on systems with slow 128bit multiplication.
LowLevelHasher := { initialized_seed : U64, state : U64 } implements [
        Hasher {
            add_bytes,
            add_u8,
            add_u16,
            add_u32,
            add_u64,
            add_u128,
            complete,
        },
    ]

# Returns a application specific pseudo random seed for Dict.
# This avoids trivial DOS attacks.
pseudo_seed : {} -> U64

create_low_level_hasher : [PseudoRandSeed, WithSeed U64] -> LowLevelHasher
create_low_level_hasher = \seed_opt ->
    seed =
        when seed_opt is
            PseudoRandSeed -> pseudo_seed({})
            WithSeed(s) -> s
    @LowLevelHasher({ initialized_seed: init_seed(seed), state: seed })

combine_state : LowLevelHasher, { a : U64, b : U64, seed : U64, length : U64 } -> LowLevelHasher
combine_state = \@LowLevelHasher({ initialized_seed, state }), { a, b, seed, length } ->
    mum =
        a
        |> Num.bitwise_xor(wyp1)
        |> wymum(Num.bitwise_xor(b, seed))
    nexta =
        mum.lower
        |> Num.bitwise_xor(wyp0)
        |> Num.bitwise_xor(length)
    nextb =
        mum.upper
        |> Num.bitwise_xor(wyp1)
    hash = wymix(nexta, nextb)

    @LowLevelHasher({ initialized_seed, state: wymix(state, hash) })

init_seed = \seed ->
    seed
    |> Num.bitwise_xor(wyp0)
    |> wymix(wyp1)
    |> Num.bitwise_xor(seed)

complete = \@LowLevelHasher({ state }) -> state

# These implementations hash each value individually with the seed and then mix
# the resulting hash with the state. There are other options that may be faster
# like using the output of the last hash as the seed to the current hash.
# I am simply not sure the tradeoffs here. Theoretically this method is more sound.
# Either way, the performance will be similar and we can change this later.
add_u8 = \@LowLevelHasher({ initialized_seed, state }), u8 ->
    p0 = Num.to_u64(u8)
    a =
        Num.shift_left_by(p0, 16)
        |> Num.bitwise_or(Num.shift_left_by(p0, 8))
        |> Num.bitwise_or(p0)
    b = 0

    combine_state(@LowLevelHasher({ initialized_seed, state }), { a, b, seed: initialized_seed, length: 1 })

add_u16 = \@LowLevelHasher({ initialized_seed, state }), u16 ->
    p0 = Num.bitwise_and(u16, 0xFF) |> Num.to_u64
    p1 = Num.shift_right_zf_by(u16, 8) |> Num.to_u64
    a =
        Num.shift_left_by(p0, 16)
        |> Num.bitwise_or(Num.shift_left_by(p1, 8))
        |> Num.bitwise_or(p1)
    b = 0

    combine_state(@LowLevelHasher({ initialized_seed, state }), { a, b, seed: initialized_seed, length: 2 })

add_u32 = \@LowLevelHasher({ initialized_seed, state }), u32 ->
    p0 = Num.to_u64(u32)
    a = Num.shift_left_by(p0, 32) |> Num.bitwise_or(p0)

    combine_state(@LowLevelHasher({ initialized_seed, state }), { a, b: a, seed: initialized_seed, length: 4 })

add_u64 = \@LowLevelHasher({ initialized_seed, state }), u64 ->
    p0 = Num.bitwise_and(0xFFFF_FFFF, u64)
    p1 = Num.shift_right_zf_by(u64, 32)
    a = Num.shift_left_by(p0, 32) |> Num.bitwise_or(p1)
    b = Num.shift_left_by(p1, 32) |> Num.bitwise_or(p0)

    combine_state(@LowLevelHasher({ initialized_seed, state }), { a, b, seed: initialized_seed, length: 8 })

add_u128 = \@LowLevelHasher({ initialized_seed, state }), u128 ->
    lower = u128 |> Num.to_u64
    upper = Num.shift_right_zf_by(u128, 64) |> Num.to_u64
    p0 = Num.bitwise_and(0xFFFF_FFFF, lower)
    p1 = Num.shift_right_zf_by(lower, 32) |> Num.bitwise_and(0xFFFF_FFFF)
    p2 = Num.bitwise_and(0xFFFF_FFFF, upper)
    p3 = Num.shift_right_zf_by(upper, 32) |> Num.bitwise_and(0xFFFF_FFFF)
    a = Num.shift_left_by(p0, 32) |> Num.bitwise_or(p2)
    b = Num.shift_left_by(p3, 32) |> Num.bitwise_or(p1)

    combine_state(@LowLevelHasher({ initialized_seed, state }), { a, b, seed: initialized_seed, length: 16 })

add_bytes : LowLevelHasher, List U8 -> LowLevelHasher
add_bytes = \@LowLevelHasher({ initialized_seed, state }), list ->
    length = List.len(list)
    abs =
        if length <= 16 then
            if length >= 4 then
                x = Num.shift_right_zf_by(length, 3) |> Num.shift_left_by(2)
                a = Num.bitwise_or(wyr4(list, 0) |> Num.shift_left_by(32), wyr4(list, x))
                b =
                    wyr4(list, Num.sub_wrap(length, 4))
                    |> Num.shift_left_by(32)
                    |> Num.bitwise_or(wyr4(list, Num.sub_wrap(length, 4) |> Num.sub_wrap(x)))

                { a, b, seed: initialized_seed }
            else if length > 0 then
                { a: wyr3(list, 0, length), b: 0, seed: initialized_seed }
            else
                { a: 0, b: 0, seed: initialized_seed }
        else if length <= 48 then
            hash_bytes_helper16(initialized_seed, list, 0, length)
        else
            hash_bytes_helper48(initialized_seed, initialized_seed, initialized_seed, list, 0, length)

    combine_state(@LowLevelHasher({ initialized_seed, state }), { a: abs.a, b: abs.b, seed: abs.seed, length })

hash_bytes_helper48 : U64, U64, U64, List U8, U64, U64 -> { a : U64, b : U64, seed : U64 }
hash_bytes_helper48 = \seed, see1, see2, list, index, remaining ->
    new_seed = wymix(Num.bitwise_xor(wyr8(list, index), wyp1), Num.bitwise_xor(wyr8(list, Num.add_wrap(index, 8)), seed))
    new_see1 = wymix(Num.bitwise_xor(wyr8(list, Num.add_wrap(index, 16)), wyp2), Num.bitwise_xor(wyr8(list, Num.add_wrap(index, 24)), see1))
    new_see2 = wymix(Num.bitwise_xor(wyr8(list, Num.add_wrap(index, 32)), wyp3), Num.bitwise_xor(wyr8(list, Num.add_wrap(index, 40)), see2))
    new_remaining = Num.sub_wrap(remaining, 48)
    new_index = Num.add_wrap(index, 48)

    if new_remaining > 48 then
        hash_bytes_helper48(new_seed, new_see1, new_see2, list, new_index, new_remaining)
    else if new_remaining > 16 then
        final_seed = Num.bitwise_xor(new_see2, Num.bitwise_xor(new_see1, new_seed))

        hash_bytes_helper16(final_seed, list, new_index, new_remaining)
    else
        final_seed = Num.bitwise_xor(new_see2, Num.bitwise_xor(new_see1, new_seed))

        { a: wyr8(list, (Num.sub_wrap(new_remaining, 16) |> Num.add_wrap(new_index))), b: wyr8(list, (Num.sub_wrap(new_remaining, 8) |> Num.add_wrap(new_index))), seed: final_seed }

hash_bytes_helper16 : U64, List U8, U64, U64 -> { a : U64, b : U64, seed : U64 }
hash_bytes_helper16 = \seed, list, index, remaining ->
    new_seed = wymix(Num.bitwise_xor(wyr8(list, index), wyp1), Num.bitwise_xor(wyr8(list, Num.add_wrap(index, 8)), seed))
    new_remaining = Num.sub_wrap(remaining, 16)
    new_index = Num.add_wrap(index, 16)

    if new_remaining <= 16 then
        { a: wyr8(list, (Num.sub_wrap(new_remaining, 16) |> Num.add_wrap(new_index))), b: wyr8(list, (Num.sub_wrap(new_remaining, 8) |> Num.add_wrap(new_index))), seed: new_seed }
    else
        hash_bytes_helper16(new_seed, list, new_index, new_remaining)

wyp0 : U64
wyp0 = 0xa0761d6478bd642f
wyp1 : U64
wyp1 = 0xe7037ed1a0b428db
wyp2 : U64
wyp2 = 0x8ebc6af09c88c6e3
wyp3 : U64
wyp3 = 0x589965cc75374cc3

wymix : U64, U64 -> U64
wymix = \a, b ->
    { lower, upper } = wymum(a, b)

    Num.bitwise_xor(lower, upper)

wymum : U64, U64 -> { lower : U64, upper : U64 }
wymum = \a, b ->
    r = Num.mul_wrap(Num.to_u128(a), Num.to_u128(b))
    lower = Num.to_u64(r)
    upper = Num.shift_right_zf_by(r, 64) |> Num.to_u64

    # This is the more robust form, which we may look into later
    # { lower: Num.bitwise_xor(a, lower), upper: Num.bitwise_xor(b, upper) }
    { lower, upper }

# Get the next 8 bytes as a U64
wyr8 : List U8, U64 -> U64
wyr8 = \list, index ->
    # With seamless slices and Num.from_bytes, this should be possible to make faster and nicer.
    # It would also deal with the fact that on big endian systems we want to invert the order here.
    # Without seamless slices, we would need from_bytes to take an index.
    p1 = list_get_unsafe(list, index) |> Num.to_u64
    p2 = list_get_unsafe(list, Num.add_wrap(index, 1)) |> Num.to_u64
    p3 = list_get_unsafe(list, Num.add_wrap(index, 2)) |> Num.to_u64
    p4 = list_get_unsafe(list, Num.add_wrap(index, 3)) |> Num.to_u64
    p5 = list_get_unsafe(list, Num.add_wrap(index, 4)) |> Num.to_u64
    p6 = list_get_unsafe(list, Num.add_wrap(index, 5)) |> Num.to_u64
    p7 = list_get_unsafe(list, Num.add_wrap(index, 6)) |> Num.to_u64
    p8 = list_get_unsafe(list, Num.add_wrap(index, 7)) |> Num.to_u64
    a = Num.bitwise_or(p1, Num.shift_left_by(p2, 8))
    b = Num.bitwise_or(Num.shift_left_by(p3, 16), Num.shift_left_by(p4, 24))
    c = Num.bitwise_or(Num.shift_left_by(p5, 32), Num.shift_left_by(p6, 40))
    d = Num.bitwise_or(Num.shift_left_by(p7, 48), Num.shift_left_by(p8, 56))

    Num.bitwise_or(Num.bitwise_or(a, b), Num.bitwise_or(c, d))

# Get the next 4 bytes as a U64 with some shifting.
wyr4 : List U8, U64 -> U64
wyr4 = \list, index ->
    p1 = list_get_unsafe(list, index) |> Num.to_u64
    p2 = list_get_unsafe(list, Num.add_wrap(index, 1)) |> Num.to_u64
    p3 = list_get_unsafe(list, Num.add_wrap(index, 2)) |> Num.to_u64
    p4 = list_get_unsafe(list, Num.add_wrap(index, 3)) |> Num.to_u64
    a = Num.bitwise_or(p1, Num.shift_left_by(p2, 8))
    b = Num.bitwise_or(Num.shift_left_by(p3, 16), Num.shift_left_by(p4, 24))

    Num.bitwise_or(a, b)

# Get the next K bytes with some shifting.
# K must be 3 or less.
wyr3 : List U8, U64, U64 -> U64
wyr3 = \list, index, k ->
    # ((uint64_t)p[0])<<16)|(((uint64_t)p[k>>1])<<8)|p[k-1]
    p1 = list_get_unsafe(list, index) |> Num.to_u64
    p2 = list_get_unsafe(list, Num.shift_right_zf_by(k, 1) |> Num.add_wrap(index)) |> Num.to_u64
    p3 = list_get_unsafe(list, Num.sub_wrap(k, 1) |> Num.add_wrap(index)) |> Num.to_u64
    a = Num.bitwise_or(Num.shift_left_by(p1, 16), Num.shift_left_by(p2, 8))

    Num.bitwise_or(a, p3)

test_seed = WithSeed(0x526F_6352_616E_643F)

# TODO: would be great to have table driven expects for this.
# Would also be great to have some sort of property based hasher
# where we can compare `add_u*` functions to the `add_bytes` function.
expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_bytes([])
        |> complete

    hash == 0xD59C59757DBBE6B3

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_bytes([0x42])
        |> complete

    hash == 0x38CE03D0E61AF963

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_u8(0x42)
        |> complete

    hash == 0x38CE03D0E61AF963

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_bytes([0xFF, 0xFF])
        |> complete

    hash == 0xE1CB2FA0D6A64113

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_u16(0xFFFF)
        |> complete

    hash == 0xE1CB2FA0D6A64113

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_bytes([0x36, 0xA7])
        |> complete

    hash == 0x26B8319EDAF81B15

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_u16(0xA736)
        |> complete

    hash == 0x26B8319EDAF81B15

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_bytes([0x00, 0x00, 0x00, 0x00])
        |> complete

    hash == 0xA187D7CA074F9EE7

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_u32(0x0000_0000)
        |> complete

    hash == 0xA187D7CA074F9EE7

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_bytes([0xA9, 0x2F, 0xEE, 0x21])
        |> complete

    hash == 0xA499EFE4C1454D09

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_u32(0x21EE_2FA9)
        |> complete

    hash == 0xA499EFE4C1454D09

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_bytes([0x5D, 0x66, 0xB1, 0x8F, 0x68, 0x44, 0xC7, 0x03, 0xE1, 0xDD, 0x23, 0x34, 0xBB, 0x9A, 0x42, 0xA7])
        |> complete

    hash == 0xDD39A206AED64C73

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_u128(0xA742_9ABB_3423_DDE1_03C7_4468_8FB1_665D)
        |> complete

    hash == 0xDD39A206AED64C73

expect
    hash =
        create_low_level_hasher(test_seed)
        |> Hash.hash_str_bytes("abcdefghijklmnopqrstuvwxyz")
        |> complete

    hash == 0x51C59DF5B1D15F40

expect
    hash =
        create_low_level_hasher(test_seed)
        |> Hash.hash_str_bytes("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
        |> complete

    hash == 0xD8D0A129D97A4E95

expect
    hash =
        create_low_level_hasher(test_seed)
        |> Hash.hash_str_bytes("1234567890123456789012345678901234567890123456789012345678901234567890")
        |> complete

    hash == 0x8188065B44FB4AAA

expect
    hash =
        create_low_level_hasher(test_seed)
        |> add_bytes(List.repeat(0x77, 100))
        |> complete

    hash == 0x47A2A606EADF3378

# Note, had to specify u8 in the lists below to avoid ability type resolution error.
# Apparently it won't pick the default integer.
expect
    hash =
        create_low_level_hasher(test_seed)
        |> Hash.hash_unordered([8u8, 82u8, 3u8, 8u8, 24u8], List.walk)
        |> complete

    hash == 0xB2E8254C08F16B20

expect
    hash1 =
        create_low_level_hasher(test_seed)
        |> Hash.hash_unordered([0u8, 1u8, 2u8, 3u8, 4u8], List.walk)
        |> complete

    hash2 =
        create_low_level_hasher(test_seed)
        |> Hash.hash_unordered([4u8, 3u8, 2u8, 1u8, 0u8], List.walk)
        |> complete

    hash1 == hash2

expect
    hash1 =
        create_low_level_hasher(test_seed)
        |> Hash.hash_unordered([0u8, 1u8, 2u8, 3u8, 4u8], List.walk)
        |> complete

    hash2 =
        create_low_level_hasher(test_seed)
        |> Hash.hash_unordered([4u8, 3u8, 2u8, 1u8, 0u8, 0u8], List.walk)
        |> complete

    hash1 != hash2

expect
    empty({})
    |> len
    |> Bool.is_eq(0)

expect
    empty({})
    |> insert("One", "A Song")
    |> insert("Two", "Candy Canes")
    |> insert("Three", "Boughs of Holly")
    |> clear
    |> len
    |> Bool.is_eq(0)

expect
    Dict.empty({})
    |> Dict.insert("Alice", 17)
    |> Dict.insert("Bob", 18)
    |> Dict.insert("Charlie", 19)
    |> Dict.walk_until(Bool.false, \_, _, age -> if age >= 18 then Break(Bool.true) else Continue(Bool.false))
    |> Bool.is_eq(Bool.true)

expect
    d1 =
        Dict.empty({})
        |> Dict.insert("Alice", 17)
        |> Dict.insert("Bob", 18)
        |> Dict.insert("Charlie", 19)
        |> Dict.keep_if(\(_k, v) -> v >= 18)

    d2 =
        Dict.empty({})
        |> Dict.insert("Bob", 18)
        |> Dict.insert("Charlie", 19)

    d1 == d2

expect
    d1 =
        Dict.empty({})
        |> Dict.insert("Alice", 17)
        |> Dict.insert("Bob", 18)
        |> Dict.insert("Charlie", 19)
        |> Dict.keep_if(\(k, _v) -> Str.ends_with(k, "e"))

    d2 =
        Dict.empty({})
        |> Dict.insert("Alice", 17)
        |> Dict.insert("Charlie", 19)

    d1 == d2

expect
    keys_to_delete = [1, 2]
    d1 =
        Dict.empty({})
        |> Dict.insert(0, 0)
        |> Dict.insert(1, 1)
        |> Dict.insert(2, 2)
        |> Dict.insert(3, 3)
        |> Dict.insert(4, 4)
        |> Dict.keep_if(\(k, _v) -> !(List.contains(keys_to_delete, k)))

    d2 =
        Dict.empty({})
        |> Dict.insert(0, 0)
        |> Dict.insert(3, 3)
        |> Dict.insert(4, 4)

    d1 == d2

expect
    keys_to_delete = [2, 4]
    d1 =
        Dict.empty({})
        |> Dict.insert(0, 0)
        |> Dict.insert(1, 1)
        |> Dict.insert(2, 2)
        |> Dict.insert(3, 3)
        |> Dict.insert(4, 4)
        |> Dict.keep_if(\(k, _v) -> !(List.contains(keys_to_delete, k)))

    d2 =
        Dict.empty({})
        |> Dict.insert(0, 0)
        |> Dict.insert(1, 1)
        |> Dict.insert(3, 3)

    d1 == d2
