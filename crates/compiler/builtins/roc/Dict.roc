interface Dict
    exposes [
        Dict,
        empty,
        withCapacity,
        single,
        clear,
        capacity,
        len,
        isEmpty,
        get,
        contains,
        insert,
        remove,
        update,
        walk,
        walkUntil,
        toList,
        fromList,
        keys,
        values,
        insertAll,
        keepShared,
        removeAll,
        map,
        joinMap,
    ]
    imports [
        Bool.{ Bool, Eq },
        Result.{ Result },
        List,
        Str,
        Num.{ Nat, U64, U8, I8 },
        Hash.{ Hasher, Hash },
    ]

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
## ```
## populationByCity =
##     Dict.empty {}
##     |> Dict.insert "London" 8_961_989
##     |> Dict.insert "Philadelphia" 1_603_797
##     |> Dict.insert "Shanghai" 24_870_895
##     |> Dict.insert "Delhi" 16_787_941
##     |> Dict.insert "Amsterdam" 872_680
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
## ```
## populationByCity
##     |> Dict.remove "Philadelphia"
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
## The internal implementation of a dictionary is similar to [absl::flat_hash_map](https://abseil.io/docs/cpp/guides/container).
## It has a list of keys value pairs that is ordered based on insertion.
## It uses a list of indices into the data as the backing of a hash map.
Dict k v := {
    # TODO: Add hashflooding ordered map fall back.
    # TODO: Add Groups and SIMD h1 key comparison (initial tests where slower, but with proper SIMD should be fast).
    # TODO: As an optimization, we can make all of these lists in one allocation
    # TODO: Grow data with the rest of the hashmap. This will require creating a list of garbage data.
    # TODO: Change remove to use tombstones. Store the tombstones in a bitmap.
    # TODO: define Eq and Hash that are unordered. Only if value implements hash/eq?
    metadata : List I8,
    dataIndices : List Nat,
    data : List (k, v),
    size : Nat,
} where k implements Hash & Eq
    implements [
        Eq {
            isEq,
        },
        Hash {
            hash: hashDict,
        },
    ]

isEq : Dict k v, Dict k v -> Bool where k implements Hash & Eq, v implements Eq
isEq = \xs, ys ->
    if len xs != len ys then
        Bool.false
    else
        walkUntil xs Bool.true \_, k, xVal ->
            when get ys k is
                Ok yVal if yVal == xVal ->
                    Continue Bool.true

                _ ->
                    Break Bool.false

hashDict : hasher, Dict k v -> hasher where k implements Hash & Eq, v implements Hash, hasher implements Hasher
hashDict = \hasher, dict -> Hash.hashUnordered hasher (toList dict) List.walk

## Return an empty dictionary.
## ```
## emptyDict = Dict.empty {}
## ```
empty : {} -> Dict * *
empty = \{} ->
    @Dict {
        metadata: List.repeat emptySlot 8,
        dataIndices: List.repeat 0 8,
        data: [],
        size: 0,
    }

## Returns the max number of elements the dictionary can hold before requiring a rehash.
## ```
## foodDict =
##     Dict.empty {}
##     |> Dict.insert "apple" "fruit"
##
## capacityOfDict = Dict.capacity foodDict
## ```
capacity : Dict * * -> Nat
capacity = \@Dict { dataIndices } ->
    cap = List.len dataIndices

    Num.subWrap cap (Num.shiftRightZfBy cap 3)

## Return a dictionary with space allocated for a number of entries. This
## may provide a performance optimization if you know how many entries will be
## inserted.
withCapacity : Nat -> Dict * *
withCapacity = \_ ->
    # TODO: power of 2 * 8 and actual implementation
    empty {}

## Returns a dictionary containing the key and value provided as input.
## ```
## expect
##     Dict.single "A" "B"
##     |> Bool.isEq (Dict.insert (Dict.empty {}) "A" "B")
## ```
single : k, v -> Dict k v where k implements Hash & Eq
single = \k, v ->
    insert (empty {}) k v

## Returns dictionary with the keys and values specified by the input [List].
## ```
## expect
##     Dict.single 1 "One"
##     |> Dict.insert 2 "Two"
##     |> Dict.insert 3 "Three"
##     |> Dict.insert 4 "Four"
##     |> Bool.isEq (Dict.fromList [(1, "One"), (2, "Two"), (3, "Three"), (4, "Four")])
## ```
fromList : List (k, v) -> Dict k v where k implements Hash & Eq
fromList = \data ->
    # TODO: make this efficient. Should just set data and then set all indicies in the hashmap.
    List.walk data (empty {}) (\dict, (k, v) -> insert dict k v)

## Returns the number of values in the dictionary.
## ```
## expect
##     Dict.empty {}
##     |> Dict.insert "One" "A Song"
##     |> Dict.insert "Two" "Candy Canes"
##     |> Dict.insert "Three" "Boughs of Holly"
##     |> Dict.len
##     |> Bool.isEq 3
## ```
len : Dict * * -> Nat
len = \@Dict { size } ->
    size

## Check if the dictinoary is empty.
## ```
## Dict.isEmpty (Dict.empty {} |> Dict.insert "key" 42)
##
## Dict.isEmpty (Dict.empty {})
## ```
isEmpty : Dict * * -> Bool
isEmpty = \@Dict { size } ->
    size == 0

## Clears all elements from a dictionary keeping around the allocation if it isn't huge.
## ```
## songs =
##        Dict.empty {}
##        |> Dict.insert "One" "A Song"
##        |> Dict.insert "Two" "Candy Canes"
##        |> Dict.insert "Three" "Boughs of Holly"
##
## clearSongs = Dict.clear songs
##
## expect Dict.len clearSongs == 0
## ```
clear : Dict k v -> Dict k v where k implements Hash & Eq
clear = \@Dict { metadata, dataIndices, data } ->
    cap = List.len dataIndices

    # Only clear large allocations.
    if cap > 128 * 8 then
        empty {}
    else
        @Dict {
            metadata: List.map metadata (\_ -> emptySlot),
            # just leave data indicies as garbage, no need to clear.
            dataIndices,
            # use takeFirst to keep around the capacity.
            data: List.takeFirst data 0,
            size: 0,
        }

## Convert each value in the dictionary to something new, by calling a conversion
## function on each of them which receives both the key and the old value. Then return a
## new dictionary containing the same keys and the converted values.
map : Dict k a, (k, a -> b) -> Dict k b where k implements Hash & Eq, b implements Hash & Eq
map = \dict, transform ->
    init = withCapacity (capacity dict)

    walk dict init \answer, k, v ->
        insert answer k (transform k v)

## Like [Dict.map], except the transformation function wraps the return value
## in a dictionary. At the end, all the dictionaries get joined together
## (using [Dict.insertAll]) into one dictionary.
##
## You may know a similar function named `concatMap` in other languages.
joinMap : Dict a b, (a, b -> Dict x y) -> Dict x y where a implements Hash & Eq, x implements Hash & Eq
joinMap = \dict, transform ->
    init = withCapacity (capacity dict) # Might be a pessimization

    walk dict init \answer, k, v ->
        insertAll answer (transform k v)

## Iterate through the keys and values in the dictionary and call the provided
## function with signature `state, k, v -> state` for each value, with an
## initial `state` value provided for the first call.
## ```
## expect
##     Dict.empty {}
##     |> Dict.insert "Apples" 12
##     |> Dict.insert "Orange" 24
##     |> Dict.walk 0 (\count, _, qty -> count + qty)
##     |> Bool.isEq 36
## ```
walk : Dict k v, state, (state, k, v -> state) -> state where k implements Hash & Eq
walk = \@Dict { data }, initialState, transform ->
    List.walk data initialState (\state, (k, v) -> transform state k v)

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
## ```
## people =
##     Dict.empty {}
##     |> Dict.insert "Alice" 17
##     |> Dict.insert "Bob" 18
##     |> Dict.insert "Charlie" 19
##
## isAdult = \_, _, age ->
##         if age >= 18 then
##             Break Bool.true
##         else
##             Continue Bool.false
##
## someoneIsAnAdult = Dict.walkUntil people Bool.false isAdult
##
## expect someoneIsAnAdult == Bool.true
## ```
walkUntil : Dict k v, state, (state, k, v -> [Continue state, Break state]) -> state where k implements Hash & Eq
walkUntil = \@Dict { data }, initialState, transform ->
    List.walkUntil data initialState (\state, (k, v) -> transform state k v)

## Get the value for a given key. If there is a value for the specified key it
## will return [Ok value], otherwise return [Err KeyNotFound].
## ```
## dictionary =
##     Dict.empty {}
##     |> Dict.insert 1 "Apple"
##     |> Dict.insert 2 "Orange"
##
## expect Dict.get dictionary 1 == Ok "Apple"
## expect Dict.get dictionary 2000 == Err KeyNotFound
## ```
get : Dict k v, k -> Result v [KeyNotFound] where k implements Hash & Eq
get = \@Dict { metadata, dataIndices, data }, key ->
    hashKey =
        createLowLevelHasher PseudoRandSeed
        |> Hash.hash key
        |> complete
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    when findIndexHelper metadata dataIndices data h2Key key probe 0 is
        Ok index ->
            dataIndex = listGetUnsafe dataIndices index
            (_, v) = listGetUnsafe data dataIndex

            Ok v

        Err NotFound ->
            Err KeyNotFound

## Check if the dictionary has a value for a specified key.
## ```
## expect
##     Dict.empty {}
##     |> Dict.insert 1234 "5678"
##     |> Dict.contains 1234
##     |> Bool.isEq Bool.true
## ```
contains : Dict k v, k -> Bool where k implements Hash & Eq
contains = \@Dict { metadata, dataIndices, data }, key ->
    hashKey =
        createLowLevelHasher PseudoRandSeed
        |> Hash.hash key
        |> complete
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    when findIndexHelper metadata dataIndices data h2Key key probe 0 is
        Ok _ ->
            Bool.true

        Err NotFound ->
            Bool.false

## Insert a value into the dictionary at a specified key.
## ```
## expect
##     Dict.empty {}
##     |> Dict.insert "Apples" 12
##     |> Dict.get "Apples"
##     |> Bool.isEq (Ok 12)
## ```
insert : Dict k v, k, v -> Dict k v where k implements Hash & Eq
insert = \@Dict { metadata, dataIndices, data, size }, key, value ->
    hashKey =
        createLowLevelHasher PseudoRandSeed
        |> Hash.hash key
        |> complete
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    when findIndexHelper metadata dataIndices data h2Key key probe 0 is
        Ok index ->
            dataIndex = listGetUnsafe dataIndices index

            @Dict {
                metadata,
                dataIndices,
                data: List.set data dataIndex (key, value),
                size,
            }

        Err NotFound ->
            # The dictionary has grown, it might need to rehash.
            rehashedDict =
                maybeRehash
                    (
                        @Dict {
                            metadata,
                            dataIndices,
                            data,
                            size: Num.addWrap size 1,
                        }
                    )

            # Need to rescan searching for the first empty or deleted cell.
            insertNotFoundHelper rehashedDict key value h1Key h2Key

## Remove a value from the dictionary for a specified key.
## ```
## expect
##     Dict.empty {}
##     |> Dict.insert "Some" "Value"
##     |> Dict.remove "Some"
##     |> Dict.len
##     |> Bool.isEq 0
## ```
remove : Dict k v, k -> Dict k v where k implements Hash & Eq
remove = \@Dict { metadata, dataIndices, data, size }, key ->
    # TODO: change this from swap remove to tombstone and test is performance is still good.
    hashKey =
        createLowLevelHasher PseudoRandSeed
        |> Hash.hash key
        |> complete
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    when findIndexHelper metadata dataIndices data h2Key key probe 0 is
        Ok index ->
            last = Num.subWrap (List.len data) 1
            dataIndex = listGetUnsafe dataIndices index

            if dataIndex == last then
                @Dict {
                    metadata: List.set metadata index deletedSlot,
                    dataIndices,
                    data: List.dropLast data,
                    size: Num.subWrap size 1,
                }
            else
                swapAndUpdateDataIndex (@Dict { metadata, dataIndices, data, size }) index last

        Err NotFound ->
            @Dict { metadata, dataIndices, data, size }

## Insert or remove a value for a specified key. This function enables a
## performance optimization for the use case of providing a default when a value
## is missing. This is more efficient than doing both a `Dict.get` and then a
## `Dict.insert` call, and supports being piped.
## ```
## alterValue : [Present Bool, Missing] -> [Present Bool, Missing]
## alterValue = \possibleValue ->
##     when possibleValue is
##         Missing -> Present Bool.false
##         Present value -> if value then Missing else Present Bool.true
##
## expect Dict.update (Dict.empty {}) "a" alterValue == Dict.single "a" Bool.false
## expect Dict.update (Dict.single "a" Bool.false) "a" alterValue == Dict.single "a" Bool.true
## expect Dict.update (Dict.single "a" Bool.true) "a" alterValue == Dict.empty {}
## ```
update : Dict k v, k, ([Present v, Missing] -> [Present v, Missing]) -> Dict k v where k implements Hash & Eq
update = \dict, key, alter ->
    # TODO: look into optimizing by merging substeps and reducing lookups.
    possibleValue =
        get dict key
        |> Result.map Present
        |> Result.withDefault Missing

    when alter possibleValue is
        Present value -> insert dict key value
        Missing -> remove dict key

## Returns the keys and values of a dictionary as a [List].
## This requires allocating a temporary list, prefer using [Dict.toList] or [Dict.walk] instead.
## ```
## expect
##     Dict.single 1 "One"
##     |> Dict.insert 2 "Two"
##     |> Dict.insert 3 "Three"
##     |> Dict.insert 4 "Four"
##     |> Dict.toList
##     |> Bool.isEq [(1, "One"), (2, "Two"), (3, "Three"), (4, "Four")]
## ```
toList : Dict k v -> List (k, v) where k implements Hash & Eq
toList = \@Dict { data } ->
    data

## Returns the keys of a dictionary as a [List].
## This requires allocating a temporary [List], prefer using [Dict.toList] or [Dict.walk] instead.
## ```
## expect
##     Dict.single 1 "One"
##     |> Dict.insert 2 "Two"
##     |> Dict.insert 3 "Three"
##     |> Dict.insert 4 "Four"
##     |> Dict.keys
##     |> Bool.isEq [1,2,3,4]
## ```
keys : Dict k v -> List k where k implements Hash & Eq
keys = \@Dict { data } ->
    List.map data (\(k, _) -> k)

## Returns the values of a dictionary as a [List].
## This requires allocating a temporary [List], prefer using [Dict.toList] or [Dict.walk] instead.
## ```
## expect
##     Dict.single 1 "One"
##     |> Dict.insert 2 "Two"
##     |> Dict.insert 3 "Three"
##     |> Dict.insert 4 "Four"
##     |> Dict.values
##     |> Bool.isEq ["One","Two","Three","Four"]
## ```
values : Dict k v -> List v where k implements Hash & Eq
values = \@Dict { data } ->
    List.map data (\(_, v) -> v)

## Combine two dictionaries by keeping the [union](https://en.wikipedia.org/wiki/Union_(set_theory))
## of all the key-value pairs. This means that all the key-value pairs in
## both dictionaries will be combined. Note that where there are pairs
## with the same key, the value contained in the second input will be
## retained, and the value in the first input will be removed.
## ```
## first =
##     Dict.single 1 "Not Me"
##     |> Dict.insert 2 "And Me"
##
## second =
##     Dict.single 1 "Keep Me"
##     |> Dict.insert 3 "Me Too"
##     |> Dict.insert 4 "And Also Me"
##
## expected =
##     Dict.single 1 "Keep Me"
##     |> Dict.insert 2 "And Me"
##     |> Dict.insert 3 "Me Too"
##     |> Dict.insert 4 "And Also Me"
##
## expect
##     Dict.insertAll first second == expected
## ```
insertAll : Dict k v, Dict k v -> Dict k v where k implements Hash & Eq
insertAll = \xs, ys ->
    walk ys xs insert

## Combine two dictionaries by keeping the [intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory))
## of all the key-value pairs. This means that we keep only those pairs
## that are in both dictionaries. Note that where there are pairs with
## the same key, the value contained in the first input will be retained,
## and the value in the second input will be removed.
## ```
## first =
##     Dict.single 1 "Keep Me"
##     |> Dict.insert 2 "And Me"
##
## second =
##     Dict.single 1 "Keep Me"
##     |> Dict.insert 2 "And Me"
##     |> Dict.insert 3 "But Not Me"
##     |> Dict.insert 4 "Or Me"
##
## expect Dict.keepShared first second == first
## ```
keepShared : Dict k v, Dict k v -> Dict k v where k implements Hash & Eq
keepShared = \xs, ys ->
    walk
        xs
        (empty {})
        (\state, k, v ->
            if contains ys k then
                insert state k v
            else
                state
        )

## Remove the key-value pairs in the first input that are also in the second
## using the [set difference](https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement)
## of the values. This means that we will be left with only those pairs that
## are in the first dictionary and whose keys are not in the second.
## ```
## first =
##     Dict.single 1 "Keep Me"
##     |> Dict.insert 2 "And Me"
##     |> Dict.insert 3 "Remove Me"
##
## second =
##     Dict.single 3 "Remove Me"
##     |> Dict.insert 4 "I do nothing..."
##
## expected =
##     Dict.single 1 "Keep Me"
##     |> Dict.insert 2 "And Me"
##
## expect Dict.removeAll first second == expected
## ```
removeAll : Dict k v, Dict k v -> Dict k v where k implements Hash & Eq
removeAll = \xs, ys ->
    walk ys xs (\state, k, _ -> remove state k)

swapAndUpdateDataIndex : Dict k v, Nat, Nat -> Dict k v where k implements Hash & Eq
swapAndUpdateDataIndex = \@Dict { metadata, dataIndices, data, size }, removedIndex, lastIndex ->
    (key, _) = listGetUnsafe data lastIndex
    hashKey =
        createLowLevelHasher PseudoRandSeed
        |> Hash.hash key
        |> complete
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    when findIndexHelper metadata dataIndices data h2Key key probe 0 is
        Ok index ->
            dataIndex = listGetUnsafe dataIndices removedIndex
            # Swap and remove data.
            nextData =
                data
                |> List.swap dataIndex lastIndex
                |> List.dropLast

            @Dict {
                # Set old metadata as deleted.
                metadata: List.set metadata removedIndex deletedSlot,
                # Update index of swaped element.
                dataIndices: List.set dataIndices index dataIndex,
                data: nextData,
                size: Num.subWrap size 1,
            }

        Err NotFound ->
            # This should be impossible.
            crash "unreachable state in dict swapAndUpdateDataIndex hit. Definitely a standard library bug."

insertNotFoundHelper : Dict k v, k, v, U64, I8 -> Dict k v
insertNotFoundHelper = \@Dict { metadata, dataIndices, data, size }, key, value, h1Key, h2Key ->
    probe = newProbe h1Key (div8 (List.len metadata))
    index = nextEmptyOrDeletedHelper metadata probe 0
    dataIndex = List.len data
    nextData = List.append data (key, value)

    @Dict {
        metadata: List.set metadata index h2Key,
        dataIndices: List.set dataIndices index dataIndex,
        data: nextData,
        size,
    }

nextEmptyOrDeletedHelper : List I8, Probe, Nat -> Nat
nextEmptyOrDeletedHelper = \metadata, probe, offset ->
    # For inserting, we can use deleted indices.
    index = Num.addWrap (mul8 probe.slotIndex) offset

    md = listGetUnsafe metadata index

    if md < 0 then
        # Empty or deleted slot, no possibility of the element.
        index
    else if offset == 7 then
        nextEmptyOrDeletedHelper metadata (nextProbe probe) 0
    else
        nextEmptyOrDeletedHelper metadata probe (Num.addWrap offset 1)

# TODO: investigate if this needs to be split into more specific helper functions.
# There is a chance that returning specific sub-info like the value would be faster.
findIndexHelper : List I8, List Nat, List (k, v), I8, k, Probe, Nat -> Result Nat [NotFound] where k implements Hash & Eq
findIndexHelper = \metadata, dataIndices, data, h2Key, key, probe, offset ->
    # For finding a value, we must search past all deleted element tombstones.
    index = Num.addWrap (mul8 probe.slotIndex) offset

    md = listGetUnsafe metadata index

    if md == emptySlot then
        # Empty slot, no possibility of the element.
        Err NotFound
    else if md == h2Key then
        # Potentially matching slot, check if the key is a match.
        dataIndex = listGetUnsafe dataIndices index
        (k, _) = listGetUnsafe data dataIndex

        if k == key then
            # We have a match, return its index.
            Ok index
        else if offset == 7 then
            # No match, keep checking.
            findIndexHelper metadata dataIndices data h2Key key (nextProbe probe) 0
        else
            findIndexHelper metadata dataIndices data h2Key key probe (Num.addWrap offset 1)
    else if offset == 7 then
        # Used slot, check next slot.
        findIndexHelper metadata dataIndices data h2Key key (nextProbe probe) 0
    else
        findIndexHelper metadata dataIndices data h2Key key probe (Num.addWrap offset 1)

# This is how we grow the container.
# If we aren't to the load factor yet, just ignore this.
# The container must have an updated size including any elements about to be inserted.
maybeRehash : Dict k v -> Dict k v where k implements Hash & Eq
maybeRehash = \@Dict { metadata, dataIndices, data, size } ->
    cap = List.len dataIndices
    maxLoadCap =
        # This is 7/8 * capacity, which is the max load factor.
        Num.subWrap cap (Num.shiftRightZfBy cap 3)

    if size > maxLoadCap then
        rehash (@Dict { metadata, dataIndices, data, size })
    else
        @Dict { metadata, dataIndices, data, size }

# TODO: switch rehash to iterate data and eventually clear out tombstones as well.
rehash : Dict k v -> Dict k v where k implements Hash & Eq
rehash = \@Dict { metadata, dataIndices, data, size } ->
    newLen = 2 * List.len dataIndices
    newDict =
        @Dict {
            metadata: List.repeat emptySlot newLen,
            dataIndices: List.repeat 0 newLen,
            data,
            size,
        }

    rehashHelper newDict metadata dataIndices data 0

rehashHelper : Dict k v, List I8, List Nat, List (k, v), Nat -> Dict k v where k implements Hash & Eq
rehashHelper = \dict, oldMetadata, oldDataIndices, oldData, index ->
    when List.get oldMetadata index is
        Ok md ->
            nextDict =
                if md >= 0 then
                    # We have an actual element here
                    dataIndex = listGetUnsafe oldDataIndices index
                    (k, _) = listGetUnsafe oldData dataIndex

                    insertForRehash dict k dataIndex
                else
                    # Empty or deleted data
                    dict

            rehashHelper nextDict oldMetadata oldDataIndices oldData (Num.addWrap index 1)

        Err OutOfBounds ->
            # Walked entire list, complete now.
            dict

insertForRehash : Dict k v, k, Nat -> Dict k v where k implements Hash & Eq
insertForRehash = \@Dict { metadata, dataIndices, data, size }, key, dataIndex ->
    hashKey =
        createLowLevelHasher PseudoRandSeed
        |> Hash.hash key
        |> complete
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))
    index = nextEmptyOrDeletedHelper metadata probe 0

    @Dict {
        metadata: List.set metadata index h2Key,
        dataIndices: List.set dataIndices index dataIndex,
        data,
        size,
    }

emptySlot : I8
emptySlot = -128
deletedSlot : I8
deletedSlot = -2

# Capacity must be a power of 2.
# We still will use slots of 8 even though this version has no true slots.
# We just move an element at a time.
# Thus, the true index is slotIndex * 8 + offset.
Probe : { slotIndex : Nat, probeI : Nat, mask : Nat }

newProbe : U64, Nat -> Probe
newProbe = \h1Key, slots ->
    mask = Num.subSaturated slots 1
    slotIndex = Num.bitwiseAnd (Num.toNat h1Key) mask

    { slotIndex, probeI: 1, mask }

nextProbe : Probe -> Probe
nextProbe = \{ slotIndex, probeI, mask } ->
    nextSlotIndex = Num.bitwiseAnd (Num.addWrap slotIndex probeI) mask

    { slotIndex: nextSlotIndex, probeI: Num.addWrap probeI 1, mask }

mul8 = \val -> Num.shiftLeftBy val 3
div8 = \val -> Num.shiftRightZfBy val 3

h1 : U64 -> U64
h1 = \hashKey ->
    Num.shiftRightZfBy hashKey 7

h2 : U64 -> I8
h2 = \hashKey ->
    Num.toI8 (Num.bitwiseAnd hashKey 0b0111_1111)

expect
    val =
        empty {}
        |> insert "foo" "bar"
        |> get "foo"

    val == Ok "bar"

expect
    dict1 =
        empty {}
        |> insert 1 "bar"
        |> insert 2 "baz"

    dict2 =
        empty {}
        |> insert 2 "baz"
        |> insert 1 "bar"

    dict1 == dict2

expect
    dict1 =
        empty {}
        |> insert 1 "bar"
        |> insert 2 "baz"

    dict2 =
        empty {}
        |> insert 1 "bar"
        |> insert 2 "baz!"

    dict1 != dict2

expect
    inner1 =
        empty {}
        |> insert 1 "bar"
        |> insert 2 "baz"

    inner2 =
        empty {}
        |> insert 2 "baz"
        |> insert 1 "bar"

    outer =
        empty {}
        |> insert inner1 "wrong"
        |> insert inner2 "right"

    get outer inner1 == Ok "right"

expect
    inner1 =
        empty {}
        |> insert 1 "bar"
        |> insert 2 "baz"

    inner2 =
        empty {}
        |> insert 2 "baz"
        |> insert 1 "bar"

    outer1 =
        empty {}
        |> insert inner1 "val"

    outer2 =
        empty {}
        |> insert inner2 "val"

    outer1 == outer2

expect
    val =
        empty {}
        |> insert "foo" "bar"
        |> insert "foo" "baz"
        |> get "foo"

    val == Ok "baz"

expect
    val =
        empty {}
        |> insert "foo" "bar"
        |> get "bar"

    val == Err KeyNotFound

expect
    empty {}
    |> insert "foo" {}
    |> contains "foo"

expect
    dict =
        empty {}
        |> insert "foo" {}
        |> insert "bar" {}
        |> insert "baz" {}

    contains dict "baz" && Bool.not (contains dict "other")

expect
    dict =
        fromList [(1u8, 1u8), (2, 2), (3, 3)]
        |> remove 1
        |> remove 3

    keys dict == [2]

expect
    list =
        fromList [(1u8, 1u8), (2u8, 2u8), (3, 3)]
        |> remove 1
        |> insert 0 0
        |> remove 3
        |> keys

    list == [0, 2]

# Reach capacity, no rehash.
expect
    val =
        empty {}
        |> insert "a" 0
        |> insert "b" 1
        |> insert "c" 2
        |> insert "d" 3
        |> insert "e" 4
        |> insert "f" 5
        |> insert "g" 6
        |> capacity

    val == 7

expect
    dict =
        empty {}
        |> insert "a" 0
        |> insert "b" 1
        |> insert "c" 2
        |> insert "d" 3
        |> insert "e" 4
        |> insert "f" 5
        |> insert "g" 6

    (get dict "a" == Ok 0)
    && (get dict "b" == Ok 1)
    && (get dict "c" == Ok 2)
    && (get dict "d" == Ok 3)
    && (get dict "e" == Ok 4)
    && (get dict "f" == Ok 5)
    && (get dict "g" == Ok 6)

# Force rehash.
expect
    val =
        empty {}
        |> insert "a" 0
        |> insert "b" 1
        |> insert "c" 2
        |> insert "d" 3
        |> insert "e" 4
        |> insert "f" 5
        |> insert "g" 6
        |> insert "h" 7
        |> capacity

    val == 14

expect
    dict =
        empty {}
        |> insert "a" 0
        |> insert "b" 1
        |> insert "c" 2
        |> insert "d" 3
        |> insert "e" 4
        |> insert "f" 5
        |> insert "g" 6
        |> insert "h" 7

    (get dict "a" == Ok 0)
    && (get dict "b" == Ok 1)
    && (get dict "c" == Ok 2)
    && (get dict "d" == Ok 3)
    && (get dict "e" == Ok 4)
    && (get dict "f" == Ok 5)
    && (get dict "g" == Ok 6)
    && (get dict "h" == Ok 7)

expect
    empty {}
    |> insert "Some" "Value"
    |> remove "Some"
    |> len
    |> Bool.isEq 0

# Makes sure a Dict with Nat keys works
expect
    empty {}
    |> insert 7nat "Testing"
    |> get 7
    |> Bool.isEq (Ok "Testing")

# We have decided not to expose the standard roc hashing algorithm.
# This is to avoid external dependence and the need for versioning.
# The current implementation is a form of [Wyhash final4](https://github.com/wangyi-fudan/wyhash/blob/77e50f267fbc7b8e2d09f2d455219adb70ad4749/wyhash.h).
# It is 64bit and little endian specific currently.
# TODO: wyhash is slow for large keys, use something like cityhash if the keys are too long.
# TODO: Add a builtin to distinguish big endian systems and change loading orders.
# TODO: Switch out Wymum on systems with slow 128bit multiplication.
LowLevelHasher := { initializedSeed : U64, state : U64 } implements [
        Hasher {
            addBytes,
            addU8,
            addU16,
            addU32,
            addU64,
            addU128,
            complete,
        },
    ]

# unsafe primitive that does not perform a bounds check
# TODO hide behind an InternalList.roc module
listGetUnsafe : List a, Nat -> a

# Returns a application specific pseudo random seed for Dict.
# This avoids trivial DOS attacks.
pseudoSeed : {} -> U64

createLowLevelHasher : [PseudoRandSeed, WithSeed U64] -> LowLevelHasher
createLowLevelHasher = \seedOpt ->
    seed =
        when seedOpt is
            PseudoRandSeed -> pseudoSeed {}
            WithSeed s -> s
    @LowLevelHasher { initializedSeed: initSeed seed, state: seed }

combineState : LowLevelHasher, { a : U64, b : U64, seed : U64, length : U64 } -> LowLevelHasher
combineState = \@LowLevelHasher { initializedSeed, state }, { a, b, seed, length } ->
    mum =
        a
        |> Num.bitwiseXor wyp1
        |> wymum (Num.bitwiseXor b seed)
    nexta =
        mum.lower
        |> Num.bitwiseXor wyp0
        |> Num.bitwiseXor length
    nextb =
        mum.upper
        |> Num.bitwiseXor wyp1
    hash = wymix nexta nextb

    @LowLevelHasher { initializedSeed, state: wymix state hash }

initSeed = \seed ->
    seed
    |> Num.bitwiseXor wyp0
    |> wymix wyp1
    |> Num.bitwiseXor seed

complete = \@LowLevelHasher { state } -> state

# These implementations hash each value individually with the seed and then mix
# the resulting hash with the state. There are other options that may be faster
# like using the output of the last hash as the seed to the current hash.
# I am simply not sure the tradeoffs here. Theoretically this method is more sound.
# Either way, the performance will be similar and we can change this later.
addU8 = \@LowLevelHasher { initializedSeed, state }, u8 ->
    p0 = Num.toU64 u8
    a =
        Num.shiftLeftBy p0 16
        |> Num.bitwiseOr (Num.shiftLeftBy p0 8)
        |> Num.bitwiseOr p0
    b = 0

    combineState (@LowLevelHasher { initializedSeed, state }) { a, b, seed: initializedSeed, length: 1 }

addU16 = \@LowLevelHasher { initializedSeed, state }, u16 ->
    p0 = Num.bitwiseAnd u16 0xFF |> Num.toU64
    p1 = Num.shiftRightZfBy u16 8 |> Num.toU64
    a =
        Num.shiftLeftBy p0 16
        |> Num.bitwiseOr (Num.shiftLeftBy p1 8)
        |> Num.bitwiseOr p1
    b = 0

    combineState (@LowLevelHasher { initializedSeed, state }) { a, b, seed: initializedSeed, length: 2 }

addU32 = \@LowLevelHasher { initializedSeed, state }, u32 ->
    p0 = Num.toU64 u32
    a = Num.shiftLeftBy p0 32 |> Num.bitwiseOr p0

    combineState (@LowLevelHasher { initializedSeed, state }) { a, b: a, seed: initializedSeed, length: 4 }

addU64 = \@LowLevelHasher { initializedSeed, state }, u64 ->
    p0 = Num.bitwiseAnd 0xFFFF_FFFF u64
    p1 = Num.shiftRightZfBy u64 32
    a = Num.shiftLeftBy p0 32 |> Num.bitwiseOr p1
    b = Num.shiftLeftBy p1 32 |> Num.bitwiseOr p0

    combineState (@LowLevelHasher { initializedSeed, state }) { a, b, seed: initializedSeed, length: 8 }

addU128 = \@LowLevelHasher { initializedSeed, state }, u128 ->
    lower = u128 |> Num.toU64
    upper = Num.shiftRightZfBy u128 64 |> Num.toU64
    p0 = Num.bitwiseAnd 0xFFFF_FFFF lower
    p1 = Num.shiftRightZfBy lower 32 |> Num.bitwiseAnd 0xFFFF_FFFF
    p2 = Num.bitwiseAnd 0xFFFF_FFFF upper
    p3 = Num.shiftRightZfBy upper 32 |> Num.bitwiseAnd 0xFFFF_FFFF
    a = Num.shiftLeftBy p0 32 |> Num.bitwiseOr p2
    b = Num.shiftLeftBy p3 32 |> Num.bitwiseOr p1

    combineState (@LowLevelHasher { initializedSeed, state }) { a, b, seed: initializedSeed, length: 16 }

addBytes : LowLevelHasher, List U8 -> LowLevelHasher
addBytes = \@LowLevelHasher { initializedSeed, state }, list ->
    length = List.len list
    abs =
        if length <= 16 then
            if length >= 4 then
                x = Num.shiftRightZfBy length 3 |> Num.shiftLeftBy 2
                a = Num.bitwiseOr (wyr4 list 0 |> Num.shiftLeftBy 32) (wyr4 list x)
                b =
                    (wyr4 list (Num.subWrap length 4) |> Num.shiftLeftBy 32)
                    |> Num.bitwiseOr (wyr4 list (Num.subWrap length 4 |> Num.subWrap x))

                { a, b, seed: initializedSeed }
            else if length > 0 then
                { a: wyr3 list 0 length, b: 0, seed: initializedSeed }
            else
                { a: 0, b: 0, seed: initializedSeed }
        else if length <= 48 then
            hashBytesHelper16 initializedSeed list 0 length
        else
            hashBytesHelper48 initializedSeed initializedSeed initializedSeed list 0 length

    combineState (@LowLevelHasher { initializedSeed, state }) { a: abs.a, b: abs.b, seed: abs.seed, length: Num.toU64 length }

hashBytesHelper48 : U64, U64, U64, List U8, Nat, Nat -> { a : U64, b : U64, seed : U64 }
hashBytesHelper48 = \seed, see1, see2, list, index, remaining ->
    newSeed = wymix (Num.bitwiseXor (wyr8 list index) wyp1) (Num.bitwiseXor (wyr8 list (Num.addWrap index 8)) seed)
    newSee1 = wymix (Num.bitwiseXor (wyr8 list (Num.addWrap index 16)) wyp2) (Num.bitwiseXor (wyr8 list (Num.addWrap index 24)) see1)
    newSee2 = wymix (Num.bitwiseXor (wyr8 list (Num.addWrap index 32)) wyp3) (Num.bitwiseXor (wyr8 list (Num.addWrap index 40)) see2)
    newRemaining = Num.subWrap remaining 48
    newIndex = Num.addWrap index 48

    if newRemaining > 48 then
        hashBytesHelper48 newSeed newSee1 newSee2 list newIndex newRemaining
    else if newRemaining > 16 then
        finalSeed = Num.bitwiseXor newSee2 (Num.bitwiseXor newSee1 newSeed)

        hashBytesHelper16 finalSeed list newIndex newRemaining
    else
        finalSeed = Num.bitwiseXor newSee2 (Num.bitwiseXor newSee1 newSeed)

        { a: wyr8 list (Num.subWrap newRemaining 16 |> Num.addWrap newIndex), b: wyr8 list (Num.subWrap newRemaining 8 |> Num.addWrap newIndex), seed: finalSeed }

hashBytesHelper16 : U64, List U8, Nat, Nat -> { a : U64, b : U64, seed : U64 }
hashBytesHelper16 = \seed, list, index, remaining ->
    newSeed = wymix (Num.bitwiseXor (wyr8 list index) wyp1) (Num.bitwiseXor (wyr8 list (Num.addWrap index 8)) seed)
    newRemaining = Num.subWrap remaining 16
    newIndex = Num.addWrap index 16

    if newRemaining <= 16 then
        { a: wyr8 list (Num.subWrap newRemaining 16 |> Num.addWrap newIndex), b: wyr8 list (Num.subWrap newRemaining 8 |> Num.addWrap newIndex), seed: newSeed }
    else
        hashBytesHelper16 newSeed list newIndex newRemaining

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
    { lower, upper } = wymum a b

    Num.bitwiseXor lower upper

wymum : U64, U64 -> { lower : U64, upper : U64 }
wymum = \a, b ->
    r = Num.toU128 a * Num.toU128 b
    lower = Num.toU64 r
    upper = Num.shiftRightZfBy r 64 |> Num.toU64

    # This is the more robust form, which we may look into later
    # { lower: Num.bitwiseXor a lower, upper: Num.bitwiseXor b upper }
    { lower, upper }

# Get the next 8 bytes as a U64
wyr8 : List U8, Nat -> U64
wyr8 = \list, index ->
    # With seamless slices and Num.fromBytes, this should be possible to make faster and nicer.
    # It would also deal with the fact that on big endian systems we want to invert the order here.
    # Without seamless slices, we would need fromBytes to take an index.
    p1 = listGetUnsafe list index |> Num.toU64
    p2 = listGetUnsafe list (Num.addWrap index 1) |> Num.toU64
    p3 = listGetUnsafe list (Num.addWrap index 2) |> Num.toU64
    p4 = listGetUnsafe list (Num.addWrap index 3) |> Num.toU64
    p5 = listGetUnsafe list (Num.addWrap index 4) |> Num.toU64
    p6 = listGetUnsafe list (Num.addWrap index 5) |> Num.toU64
    p7 = listGetUnsafe list (Num.addWrap index 6) |> Num.toU64
    p8 = listGetUnsafe list (Num.addWrap index 7) |> Num.toU64
    a = Num.bitwiseOr p1 (Num.shiftLeftBy p2 8)
    b = Num.bitwiseOr (Num.shiftLeftBy p3 16) (Num.shiftLeftBy p4 24)
    c = Num.bitwiseOr (Num.shiftLeftBy p5 32) (Num.shiftLeftBy p6 40)
    d = Num.bitwiseOr (Num.shiftLeftBy p7 48) (Num.shiftLeftBy p8 56)

    Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

# Get the next 4 bytes as a U64 with some shifting.
wyr4 : List U8, Nat -> U64
wyr4 = \list, index ->
    p1 = listGetUnsafe list index |> Num.toU64
    p2 = listGetUnsafe list (Num.addWrap index 1) |> Num.toU64
    p3 = listGetUnsafe list (Num.addWrap index 2) |> Num.toU64
    p4 = listGetUnsafe list (Num.addWrap index 3) |> Num.toU64
    a = Num.bitwiseOr p1 (Num.shiftLeftBy p2 8)
    b = Num.bitwiseOr (Num.shiftLeftBy p3 16) (Num.shiftLeftBy p4 24)

    Num.bitwiseOr a b

# Get the next K bytes with some shifting.
# K must be 3 or less.
wyr3 : List U8, Nat, Nat -> U64
wyr3 = \list, index, k ->
    # ((uint64_t)p[0])<<16)|(((uint64_t)p[k>>1])<<8)|p[k-1]
    p1 = listGetUnsafe list index |> Num.toU64
    p2 = listGetUnsafe list (Num.shiftRightZfBy k 1 |> Num.addWrap index) |> Num.toU64
    p3 = listGetUnsafe list (Num.subWrap k 1 |> Num.addWrap index) |> Num.toU64
    a = Num.bitwiseOr (Num.shiftLeftBy p1 16) (Num.shiftLeftBy p2 8)

    Num.bitwiseOr a p3

testSeed = WithSeed 0x526F_6352_616E_643F

# TODO: would be great to have table driven expects for this.
# Would also be great to have some sort of property based hasher
# where we can compare `addU*` functions to the `addBytes` function.
expect
    hash =
        createLowLevelHasher testSeed
        |> addBytes []
        |> complete

    hash == 0xD59C59757DBBE6B3

expect
    hash =
        createLowLevelHasher testSeed
        |> addBytes [0x42]
        |> complete

    hash == 0x38CE03D0E61AF963

expect
    hash =
        createLowLevelHasher testSeed
        |> addU8 0x42
        |> complete

    hash == 0x38CE03D0E61AF963

expect
    hash =
        createLowLevelHasher testSeed
        |> addBytes [0xFF, 0xFF]
        |> complete

    hash == 0xE1CB2FA0D6A64113

expect
    hash =
        createLowLevelHasher testSeed
        |> addU16 0xFFFF
        |> complete

    hash == 0xE1CB2FA0D6A64113

expect
    hash =
        createLowLevelHasher testSeed
        |> addBytes [0x36, 0xA7]
        |> complete

    hash == 0x26B8319EDAF81B15

expect
    hash =
        createLowLevelHasher testSeed
        |> addU16 0xA736
        |> complete

    hash == 0x26B8319EDAF81B15

expect
    hash =
        createLowLevelHasher testSeed
        |> addBytes [0x00, 0x00, 0x00, 0x00]
        |> complete

    hash == 0xA187D7CA074F9EE7

expect
    hash =
        createLowLevelHasher testSeed
        |> addU32 0x0000_0000
        |> complete

    hash == 0xA187D7CA074F9EE7

expect
    hash =
        createLowLevelHasher testSeed
        |> addBytes [0xA9, 0x2F, 0xEE, 0x21]
        |> complete

    hash == 0xA499EFE4C1454D09

expect
    hash =
        createLowLevelHasher testSeed
        |> addU32 0x21EE_2FA9
        |> complete

    hash == 0xA499EFE4C1454D09

expect
    hash =
        createLowLevelHasher testSeed
        |> addBytes [0x5D, 0x66, 0xB1, 0x8F, 0x68, 0x44, 0xC7, 0x03, 0xE1, 0xDD, 0x23, 0x34, 0xBB, 0x9A, 0x42, 0xA7]
        |> complete

    hash == 0xDD39A206AED64C73

expect
    hash =
        createLowLevelHasher testSeed
        |> addU128 0xA742_9ABB_3423_DDE1_03C7_4468_8FB1_665D
        |> complete

    hash == 0xDD39A206AED64C73

expect
    hash =
        createLowLevelHasher testSeed
        |> Hash.hashStrBytes "abcdefghijklmnopqrstuvwxyz"
        |> complete

    hash == 0x51C59DF5B1D15F40

expect
    hash =
        createLowLevelHasher testSeed
        |> Hash.hashStrBytes "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        |> complete

    hash == 0xD8D0A129D97A4E95

expect
    hash =
        createLowLevelHasher testSeed
        |> Hash.hashStrBytes "1234567890123456789012345678901234567890123456789012345678901234567890"
        |> complete

    hash == 0x8188065B44FB4AAA

expect
    hash =
        createLowLevelHasher testSeed
        |> addBytes (List.repeat 0x77 100)
        |> complete

    hash == 0x47A2A606EADF3378

# Note, had to specify u8 in the lists below to avoid ability type resolution error.
# Apparently it won't pick the default integer.
expect
    hash =
        createLowLevelHasher testSeed
        |> Hash.hashUnordered [8u8, 82u8, 3u8, 8u8, 24u8] List.walk
        |> complete

    hash == 0xB2E8254C08F16B20

expect
    hash1 =
        createLowLevelHasher testSeed
        |> Hash.hashUnordered ([0u8, 1u8, 2u8, 3u8, 4u8]) List.walk
        |> complete

    hash2 =
        createLowLevelHasher testSeed
        |> Hash.hashUnordered [4u8, 3u8, 2u8, 1u8, 0u8] List.walk
        |> complete

    hash1 == hash2

expect
    hash1 =
        createLowLevelHasher testSeed
        |> Hash.hashUnordered [0u8, 1u8, 2u8, 3u8, 4u8] List.walk
        |> complete

    hash2 =
        createLowLevelHasher testSeed
        |> Hash.hashUnordered [4u8, 3u8, 2u8, 1u8, 0u8, 0u8] List.walk
        |> complete

    hash1 != hash2

expect
    empty {}
    |> len
    |> Bool.isEq 0

expect
    empty {}
    |> insert "One" "A Song"
    |> insert "Two" "Candy Canes"
    |> insert "Three" "Boughs of Holly"
    |> clear
    |> len
    |> Bool.isEq 0

expect
    Dict.empty {}
    |> Dict.insert "Alice" 17
    |> Dict.insert "Bob" 18
    |> Dict.insert "Charlie" 19
    |> Dict.walkUntil Bool.false (\_, _, age -> if age >= 18 then Break Bool.true else Continue Bool.false)
    |> Bool.isEq Bool.true
