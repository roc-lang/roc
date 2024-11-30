module [
    Dict,
    empty,
    withCapacity,
    single,
    clear,
    capacity,
    reserve,
    releaseExcessCapacity,
    len,
    isEmpty,
    get,
    contains,
    insert,
    remove,
    update,
    walk,
    walkUntil,
    keepIf,
    dropIf,
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
## ```roc
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
## The internal implementation of a dictionary is almost identical to [ankerl::unordered_dense](https://github.com/martinus/unordered_dense).
## It has a list of keys value pairs that is ordered based on insertion.
## It uses a list of indices into the data as the backing of a hash map.
Dict k v := {
    buckets : List Bucket,
    data : List (k, v),
    maxBucketCapacity : U64,
    maxLoadFactor : F32,
    shifts : U8,
} where k implements Hash & Eq
    implements [
        Eq {
            isEq,
        },
        Hash {
            hash: hashDict,
        },
        Inspect {
            toInspector: toInspectorDict,
        },
    ]

isEq : Dict k v, Dict k v -> Bool where v implements Eq
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

hashDict : hasher, Dict k v -> hasher where v implements Hash, hasher implements Hasher
hashDict = \hasher, dict -> Hash.hashUnordered hasher (toList dict) List.walk

toInspectorDict : Dict k v -> Inspector f where k implements Inspect & Hash & Eq, v implements Inspect, f implements InspectFormatter
toInspectorDict = \dict ->
    Inspect.custom \fmt ->
        Inspect.apply (Inspect.dict dict walk Inspect.toInspector Inspect.toInspector) fmt

## Return an empty dictionary.
## ```roc
## emptyDict = Dict.empty {}
## ```
empty : {} -> Dict * *
empty = \{} ->
    @Dict {
        buckets: [],
        data: [],
        maxBucketCapacity: 0,
        maxLoadFactor: defaultMaxLoadFactor,
        shifts: initialShifts,
    }

## Return a dictionary with space allocated for a number of entries. This
## may provide a performance optimization if you know how many entries will be
## inserted.
withCapacity : U64 -> Dict * *
withCapacity = \requested ->
    empty {}
    |> reserve requested

## Enlarge the dictionary for at least capacity additional elements
reserve : Dict k v, U64 -> Dict k v
reserve = \@Dict { buckets, data, maxBucketCapacity: originalMaxBucketCapacity, maxLoadFactor, shifts }, requested ->
    currentSize = List.len data
    requestedSize = Num.addWrap currentSize requested
    size = Num.min requestedSize maxSize

    requestedShifts = calcShiftsForSize size maxLoadFactor
    if (List.isEmpty buckets) || requestedShifts > shifts then
        (buckets0, maxBucketCapacity) = allocBucketsFromShift requestedShifts maxLoadFactor
        buckets1 = fillBucketsFromData buckets0 data requestedShifts
        @Dict {
            buckets: buckets1,
            data: List.reserve data (Num.subSaturated size currentSize),
            maxBucketCapacity,
            maxLoadFactor,
            shifts: requestedShifts,
        }
    else
        @Dict { buckets, data, maxBucketCapacity: originalMaxBucketCapacity, maxLoadFactor, shifts }

## Shrink the memory footprint of a dictionary such that capacity is as small as possible.
## This function will require regenerating the metadata if the size changes.
## There will still be some overhead due to dictionary metadata always being a power of 2.
releaseExcessCapacity : Dict k v -> Dict k v
releaseExcessCapacity = \@Dict { buckets, data, maxBucketCapacity: originalMaxBucketCapacity, maxLoadFactor, shifts } ->
    size = List.len data

    # NOTE: If we want, we technically could increase the load factor here to potentially minimize size more.
    minShifts = calcShiftsForSize size maxLoadFactor
    if minShifts < shifts then
        (buckets0, maxBucketCapacity) = allocBucketsFromShift minShifts maxLoadFactor
        buckets1 = fillBucketsFromData buckets0 data minShifts
        @Dict {
            buckets: buckets1,
            data: List.releaseExcessCapacity data,
            maxBucketCapacity,
            maxLoadFactor,
            shifts: minShifts,
        }
    else
        @Dict { buckets, data, maxBucketCapacity: originalMaxBucketCapacity, maxLoadFactor, shifts }

## Returns the max number of elements the dictionary can hold before requiring a rehash.
## ```roc
## foodDict =
##     Dict.empty {}
##     |> Dict.insert "apple" "fruit"
##
## capacityOfDict = Dict.capacity foodDict
## ```
capacity : Dict * * -> U64
capacity = \@Dict { maxBucketCapacity } ->
    maxBucketCapacity

## Returns a dictionary containing the key and value provided as input.
## ```roc
## expect
##     Dict.single "A" "B"
##     |> Bool.isEq (Dict.insert (Dict.empty {}) "A" "B")
## ```
single : k, v -> Dict k v
single = \k, v ->
    insert (empty {}) k v

## Returns dictionary with the keys and values specified by the input [List].
## ```roc
## expect
##     Dict.single 1 "One"
##     |> Dict.insert 2 "Two"
##     |> Dict.insert 3 "Three"
##     |> Dict.insert 4 "Four"
##     |> Bool.isEq (Dict.fromList [(1, "One"), (2, "Two"), (3, "Three"), (4, "Four")])
## ```
##
## ## Performance Details
##
## This will build up from an empty dictionary to minimize totally memory use.
## If the list has few duplicate keys, it would be faster to allocate a dictionary
## with the same capacity of the list and walk it calling [Dict.insert]
fromList : List (k, v) -> Dict k v
fromList = \data ->
    List.walk data (empty {}) (\dict, (k, v) -> insert dict k v)

## Returns the number of values in the dictionary.
## ```roc
## expect
##     Dict.empty {}
##     |> Dict.insert "One" "A Song"
##     |> Dict.insert "Two" "Candy Canes"
##     |> Dict.insert "Three" "Boughs of Holly"
##     |> Dict.len
##     |> Bool.isEq 3
## ```
len : Dict * * -> U64
len = \@Dict { data } ->
    List.len data

## Check if the dictionary is empty.
## ```roc
## Dict.isEmpty (Dict.empty {} |> Dict.insert "key" 42)
##
## Dict.isEmpty (Dict.empty {})
## ```
isEmpty : Dict * * -> Bool
isEmpty = \@Dict { data } ->
    List.isEmpty data

## Clears all elements from a dictionary keeping around the allocation if it isn't huge.
## ```roc
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
clear : Dict k v -> Dict k v
clear = \@Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts } ->
    @Dict {
        buckets: List.map buckets \_ -> emptyBucket,
        # use takeFirst to keep around the capacity
        data: List.takeFirst data 0,
        maxBucketCapacity,
        maxLoadFactor,
        shifts,
    }

## Convert each value in the dictionary to something new, by calling a conversion
## function on each of them which receives both the key and the old value. Then return a
## new dictionary containing the same keys and the converted values.
map : Dict k a, (k, a -> b) -> Dict k b
map = \dict, transform ->
    init = withCapacity (capacity dict)

    walk dict init \answer, k, v ->
        insert answer k (transform k v)

## Like [Dict.map], except the transformation function wraps the return value
## in a dictionary. At the end, all the dictionaries get joined together
## (using [Dict.insertAll]) into one dictionary.
##
## You may know a similar function named `concatMap` in other languages.
joinMap : Dict a b, (a, b -> Dict x y) -> Dict x y
joinMap = \dict, transform ->
    init = withCapacity (capacity dict) # Might be a pessimization

    walk dict init \answer, k, v ->
        insertAll answer (transform k v)

## Iterate through the keys and values in the dictionary and call the provided
## function with signature `state, k, v -> state` for each value, with an
## initial `state` value provided for the first call.
## ```roc
## expect
##     Dict.empty {}
##     |> Dict.insert "Apples" 12
##     |> Dict.insert "Orange" 24
##     |> Dict.walk 0 (\count, _, qty -> count + qty)
##     |> Bool.isEq 36
## ```
walk : Dict k v, state, (state, k, v -> state) -> state
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
## ```roc
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
walkUntil : Dict k v, state, (state, k, v -> [Continue state, Break state]) -> state
walkUntil = \@Dict { data }, initialState, transform ->
    List.walkUntil data initialState (\state, (k, v) -> transform state k v)

## Run the given function on each key-value pair of a dictionary, and return
## a dictionary with just the pairs for which the function returned `Bool.true`.
## ```roc
## expect Dict.empty {}
##     |> Dict.insert "Alice" 17
##     |> Dict.insert "Bob" 18
##     |> Dict.insert "Charlie" 19
##     |> Dict.keepIf \(_k, v) -> v >= 18
##     |> Dict.len
##     |> Bool.isEq 2
## ```
keepIf : Dict k v, ((k, v) -> Bool) -> Dict k v
keepIf = \dict, predicate ->
    keepIfHelp dict predicate 0 (Dict.len dict)

keepIfHelp : Dict k v, ((k, v) -> Bool), U64, U64 -> Dict k v
keepIfHelp = \@Dict dict, predicate, index, length ->
    if index < length then
        (key, value) = listGetUnsafe dict.data index
        if predicate (key, value) then
            keepIfHelp (@Dict dict) predicate (index |> Num.addWrap 1) length
        else
            keepIfHelp (Dict.remove (@Dict dict) key) predicate index (length |> Num.subWrap 1)
    else
        @Dict dict

## Run the given function on each key-value pair of a dictionary, and return
## a dictionary with just the pairs for which the function returned `Bool.false`.
## ```roc
## expect Dict.empty {}
##     |> Dict.insert "Alice" 17
##     |> Dict.insert "Bob" 18
##     |> Dict.insert "Charlie" 19
##     |> Dict.dropIf \(_k, v) -> v >= 18
##     |> Dict.len
##     |> Bool.isEq 1
## ```
dropIf : Dict k v, ((k, v) -> Bool) -> Dict k v
dropIf = \dict, predicate ->
    Dict.keepIf dict (\e -> Bool.not (predicate e))

## Get the value for a given key. If there is a value for the specified key it
## will return [Ok value], otherwise return [Err KeyNotFound].
## ```roc
## dictionary =
##     Dict.empty {}
##     |> Dict.insert 1 "Apple"
##     |> Dict.insert 2 "Orange"
##
## expect Dict.get dictionary 1 == Ok "Apple"
## expect Dict.get dictionary 2000 == Err KeyNotFound
## ```
get : Dict k v, k -> Result v [KeyNotFound]
get = \dict, key ->
    find dict key
    |> .result

## Check if the dictionary has a value for a specified key.
## ```roc
## expect
##     Dict.empty {}
##     |> Dict.insert 1234 "5678"
##     |> Dict.contains 1234
##     |> Bool.isEq Bool.true
## ```
contains : Dict k v, k -> Bool
contains = \dict, key ->
    find dict key
    |> .result
    |> Result.isOk

## Insert a value into the dictionary at a specified key.
## ```roc
## expect
##     Dict.empty {}
##     |> Dict.insert "Apples" 12
##     |> Dict.get "Apples"
##     |> Bool.isEq (Ok 12)
## ```
insert : Dict k v, k, v -> Dict k v
insert = \dict, key, value ->
    (@Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }) =
        if len dict < capacity dict then
            dict
        else
            increaseSize dict

    hash = hashKey key
    distAndFingerprint = distAndFingerprintFromHash hash
    bucketIndex = bucketIndexFromHash hash shifts

    insertHelper buckets data bucketIndex distAndFingerprint key value maxBucketCapacity maxLoadFactor shifts

insertHelper : List Bucket, List (k, v), U64, U32, k, v, U64, F32, U8 -> Dict k v
insertHelper = \buckets0, data0, bucketIndex0, distAndFingerprint0, key, value, maxBucketCapacity, maxLoadFactor, shifts ->
    loaded = listGetUnsafe buckets0 bucketIndex0
    if distAndFingerprint0 == loaded.distAndFingerprint then
        (foundKey, _) = listGetUnsafe data0 (Num.toU64 loaded.dataIndex)
        if foundKey == key then
            data1 = List.set data0 (Num.toU64 loaded.dataIndex) (key, value)
            @Dict { buckets: buckets0, data: data1, maxBucketCapacity, maxLoadFactor, shifts }
        else
            bucketIndex1 = nextBucketIndex bucketIndex0 (List.len buckets0)
            distAndFingerprint1 = incrementDist distAndFingerprint0
            insertHelper buckets0 data0 bucketIndex1 distAndFingerprint1 key value maxBucketCapacity maxLoadFactor shifts
    else if distAndFingerprint0 > loaded.distAndFingerprint then
        data1 = List.append data0 (key, value)
        dataIndex = (List.len data1) |> Num.subWrap 1
        buckets1 = placeAndShiftUp buckets0 { distAndFingerprint: distAndFingerprint0, dataIndex: Num.toU32 dataIndex } bucketIndex0
        @Dict { buckets: buckets1, data: data1, maxBucketCapacity, maxLoadFactor, shifts }
    else
        bucketIndex1 = nextBucketIndex bucketIndex0 (List.len buckets0)
        distAndFingerprint1 = incrementDist distAndFingerprint0
        insertHelper buckets0 data0 bucketIndex1 distAndFingerprint1 key value maxBucketCapacity maxLoadFactor shifts

## Remove a value from the dictionary for a specified key.
## ```roc
## expect
##     Dict.empty {}
##     |> Dict.insert "Some" "Value"
##     |> Dict.remove "Some"
##     |> Dict.len
##     |> Bool.isEq 0
## ```
remove : Dict k v, k -> Dict k v
remove = \@Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }, key ->
    if !(List.isEmpty data) then
        (bucketIndex0, distAndFingerprint0) = nextWhileLess buckets key shifts
        (bucketIndex1, distAndFingerprint1) = removeHelper buckets bucketIndex0 distAndFingerprint0 data key

        bucket = listGetUnsafe buckets bucketIndex1
        if distAndFingerprint1 != bucket.distAndFingerprint then
            @Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }
        else
            removeBucket (@Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }) bucketIndex1
    else
        @Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }

removeHelper : List Bucket, U64, U32, List (k, *), k -> (U64, U32) where k implements Eq
removeHelper = \buckets, bucketIndex, distAndFingerprint, data, key ->
    bucket = listGetUnsafe buckets bucketIndex
    if distAndFingerprint == bucket.distAndFingerprint then
        (foundKey, _) = listGetUnsafe data (Num.toU64 bucket.dataIndex)
        if foundKey == key then
            (bucketIndex, distAndFingerprint)
        else
            removeHelper buckets (nextBucketIndex bucketIndex (List.len buckets)) (incrementDist distAndFingerprint) data key
    else
        (bucketIndex, distAndFingerprint)

## Insert or remove a value for a specified key. This function enables a
## performance optimization for the use case of providing a default when a value
## is missing. This is more efficient than doing both a `Dict.get` and then a
## `Dict.insert` call, and supports being piped.
## ```roc
## alterValue : Result Bool [Missing] -> Result Bool [Missing]
## alterValue = \possibleValue ->
##     when possibleValue is
##         Err Missing -> Ok Bool.false
##         Ok value -> if value then Err Missing else Ok Bool.true
##
## expect Dict.update (Dict.empty {}) "a" alterValue == Dict.single "a" Bool.false
## expect Dict.update (Dict.single "a" Bool.false) "a" alterValue == Dict.single "a" Bool.true
## expect Dict.update (Dict.single "a" Bool.true) "a" alterValue == Dict.empty {}
## ```
update : Dict k v, k, (Result v [Missing] -> Result v [Missing]) -> Dict k v
update = \@Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }, key, alter ->
    { bucketIndex, result } = find (@Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }) key
    when result is
        Ok value ->
            when alter (Ok value) is
                Ok newValue ->
                    bucket = listGetUnsafe buckets bucketIndex
                    newData = List.set data (Num.toU64 bucket.dataIndex) (key, newValue)
                    @Dict { buckets, data: newData, maxBucketCapacity, maxLoadFactor, shifts }

                Err Missing ->
                    removeBucket (@Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }) bucketIndex

        Err KeyNotFound ->
            when alter (Err Missing) is
                Ok newValue ->
                    if List.len data >= maxBucketCapacity then
                        # Need to reallocate let regular insert handle that.
                        insert (@Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }) key newValue
                    else
                        # Can skip work by jumping staight to the found bucket.
                        # That will be the location we want to insert in.
                        hash = hashKey key
                        baseDistAndFingerprint = distAndFingerprintFromHash hash
                        baseBucketIndex = bucketIndexFromHash hash shifts

                        # Due to the unrolling of loops in find along with loop optimizations,
                        # The bucketIndex is not guaranteed to be correct here.
                        # It is only correct if we have traversed past the number of find unrolls.
                        dist = circularDist baseBucketIndex bucketIndex (List.len buckets)
                        if dist <= findManualUnrolls then
                            insertHelper buckets data baseBucketIndex baseDistAndFingerprint key newValue maxBucketCapacity maxLoadFactor shifts
                        else
                            distAndFingerprint = incrementDistN baseDistAndFingerprint (Num.toU32 dist)
                            insertHelper buckets data bucketIndex distAndFingerprint key newValue maxBucketCapacity maxLoadFactor shifts

                Err Missing ->
                    @Dict { buckets, data, maxBucketCapacity, maxLoadFactor, shifts }

circularDist = \start, end, size ->
    correction =
        if start > end then
            size
        else
            0
    end
    |> Num.subWrap start
    |> Num.addWrap correction

## Returns the keys and values of a dictionary as a [List].
## This requires allocating a temporary list, prefer using [Dict.toList] or [Dict.walk] instead.
## ```roc
## expect
##     Dict.single 1 "One"
##     |> Dict.insert 2 "Two"
##     |> Dict.insert 3 "Three"
##     |> Dict.insert 4 "Four"
##     |> Dict.toList
##     |> Bool.isEq [(1, "One"), (2, "Two"), (3, "Three"), (4, "Four")]
## ```
toList : Dict k v -> List (k, v)
toList = \@Dict { data } ->
    data

## Returns the keys of a dictionary as a [List].
## This requires allocating a temporary [List], prefer using [Dict.toList] or [Dict.walk] instead.
## ```roc
## expect
##     Dict.single 1 "One"
##     |> Dict.insert 2 "Two"
##     |> Dict.insert 3 "Three"
##     |> Dict.insert 4 "Four"
##     |> Dict.keys
##     |> Bool.isEq [1,2,3,4]
## ```
keys : Dict k v -> List k
keys = \@Dict { data } ->
    List.map data (\(k, _) -> k)

## Returns the values of a dictionary as a [List].
## This requires allocating a temporary [List], prefer using [Dict.toList] or [Dict.walk] instead.
## ```roc
## expect
##     Dict.single 1 "One"
##     |> Dict.insert 2 "Two"
##     |> Dict.insert 3 "Three"
##     |> Dict.insert 4 "Four"
##     |> Dict.values
##     |> Bool.isEq ["One","Two","Three","Four"]
## ```
values : Dict k v -> List v
values = \@Dict { data } ->
    List.map data (\(_, v) -> v)

## Combine two dictionaries by keeping the [union](https://en.wikipedia.org/wiki/Union_(set_theory))
## of all the key-value pairs. This means that all the key-value pairs in
## both dictionaries will be combined. Note that where there are pairs
## with the same key, the value contained in the second input will be
## retained, and the value in the first input will be removed.
## ```roc
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
insertAll : Dict k v, Dict k v -> Dict k v
insertAll = \xs, ys ->
    if len ys > len xs then
        insertAll ys xs
    else
        walk ys xs insert

## Combine two dictionaries by keeping the [intersection](https://en.wikipedia.org/wiki/Intersection_(set_theory))
## of all the key-value pairs. This means that we keep only those pairs
## that are in both dictionaries. Both the key and value must match to be kept.
## ```roc
## first =
##     Dict.single 1 "Keep Me"
##     |> Dict.insert 2 "And Me"
##     |> Dict.insert 3 "Not this one"
##
## second =
##     Dict.single 1 "Keep Me"
##     |> Dict.insert 2 "And Me"
##     |> Dict.insert 3 "This has a different value"
##     |> Dict.insert 4 "Or Me"
##
## expected =
##     Dict.single 1 "Keep Me"
##     |> Dict.insert 2 "And Me"
##
## expect Dict.keepShared first second == expected
## ```
keepShared : Dict k v, Dict k v -> Dict k v where v implements Eq
keepShared = \xs0, ys0 ->
    (xs1, ys1) =
        if len ys0 < len xs0 then
            (ys0, xs0)
        else
            (xs0, ys0)
    walk
        xs1
        (withCapacity (len xs1))
        (\state, k, v ->
            when get ys1 k is
                Ok yv if v == yv ->
                    insert state k v

                _ ->
                    state
        )

## Remove the key-value pairs in the first input that are also in the second
## using the [set difference](https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement)
## of the values. This means that we will be left with only those pairs that
## are in the first dictionary and whose keys are not in the second.
## ```roc
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
removeAll : Dict k v, Dict k v -> Dict k v
removeAll = \xs, ys ->
    walk ys xs (\state, k, _ -> remove state k)

# Below here is a list of generic helpers and internal data types for Dict
Bucket : {
    distAndFingerprint : U32, # upper 3 byte: distance to original bucket. lower byte: fingerprint from hash
    dataIndex : U32, # index into the data list.
}

emptyBucket = { distAndFingerprint: 0, dataIndex: 0 }
distInc = Num.shiftLeftBy 1u32 8 # skip 1 byte fingerprint
fingerprintMask = Num.subWrap distInc 1 # mask for 1 byte of fingerprint
defaultMaxLoadFactor = 0.8
initialShifts = 64 |> Num.subWrap 3 # 2^(64-shifts) number of buckets
maxSize = Num.shiftLeftBy 1u64 32
maxBucketCount = maxSize

incrementDist = \distAndFingerprint ->
    Num.addWrap distAndFingerprint distInc

incrementDistN = \distAndFingerprint, n ->
    Num.addWrap distAndFingerprint (Num.mulWrap n distInc)

decrementDist = \distAndFingerprint ->
    distAndFingerprint |> Num.subWrap distInc

find : Dict k v, k -> { bucketIndex : U64, result : Result v [KeyNotFound] }
find = \@Dict { buckets, data, shifts }, key ->
    hash = hashKey key
    distAndFingerprint = distAndFingerprintFromHash hash
    bucketIndex = bucketIndexFromHash hash shifts

    if !(List.isEmpty data) then
        # TODO: this is true in the C++ code, confirm it in Roc as well.
        # unrolled loop. *Always* check a few directly, then enter the loop. This is faster.
        findFirstUnroll buckets bucketIndex distAndFingerprint data key
    else
        { bucketIndex, result: Err KeyNotFound }

findManualUnrolls = 2

findFirstUnroll : List Bucket, U64, U32, List (k, v), k -> { bucketIndex : U64, result : Result v [KeyNotFound] } where k implements Eq
findFirstUnroll = \buckets, bucketIndex, distAndFingerprint, data, key ->
    # TODO: once we have short circuit evaluation, use it here and other similar locations in this file.
    # Avoid the nested if with else block inconvenience.
    bucket = listGetUnsafe buckets bucketIndex
    if distAndFingerprint == bucket.distAndFingerprint then
        (foundKey, value) = listGetUnsafe data (Num.toU64 bucket.dataIndex)
        if foundKey == key then
            { bucketIndex, result: Ok value }
        else
            findSecondUnroll buckets (nextBucketIndex bucketIndex (List.len buckets)) (incrementDist distAndFingerprint) data key
    else
        findSecondUnroll buckets (nextBucketIndex bucketIndex (List.len buckets)) (incrementDist distAndFingerprint) data key

findSecondUnroll : List Bucket, U64, U32, List (k, v), k -> { bucketIndex : U64, result : Result v [KeyNotFound] } where k implements Eq
findSecondUnroll = \buckets, bucketIndex, distAndFingerprint, data, key ->
    bucket = listGetUnsafe buckets bucketIndex
    if distAndFingerprint == bucket.distAndFingerprint then
        (foundKey, value) = listGetUnsafe data (Num.toU64 bucket.dataIndex)
        if foundKey == key then
            { bucketIndex, result: Ok value }
        else
            findHelper buckets (nextBucketIndex bucketIndex (List.len buckets)) (incrementDist distAndFingerprint) data key
    else
        findHelper buckets (nextBucketIndex bucketIndex (List.len buckets)) (incrementDist distAndFingerprint) data key

findHelper : List Bucket, U64, U32, List (k, v), k -> { bucketIndex : U64, result : Result v [KeyNotFound] } where k implements Eq
findHelper = \buckets, bucketIndex, distAndFingerprint, data, key ->
    bucket = listGetUnsafe buckets bucketIndex
    if distAndFingerprint == bucket.distAndFingerprint then
        (foundKey, value) = listGetUnsafe data (Num.toU64 bucket.dataIndex)
        if foundKey == key then
            { bucketIndex, result: Ok value }
        else
            findHelper buckets (nextBucketIndex bucketIndex (List.len buckets)) (incrementDist distAndFingerprint) data key
    else if distAndFingerprint > bucket.distAndFingerprint then
        { bucketIndex, result: Err KeyNotFound }
    else
        findHelper buckets (nextBucketIndex bucketIndex (List.len buckets)) (incrementDist distAndFingerprint) data key

removeBucket : Dict k v, U64 -> Dict k v
removeBucket = \@Dict { buckets: buckets0, data: data0, maxBucketCapacity, maxLoadFactor, shifts }, bucketIndex0 ->
    dataIndexToRemove = (listGetUnsafe buckets0 bucketIndex0).dataIndex
    dataIndexToRemoveU64 = Num.toU64 dataIndexToRemove

    (buckets1, bucketIndex1) = removeBucketHelper buckets0 bucketIndex0
    buckets2 = List.set buckets1 bucketIndex1 emptyBucket

    lastDataIndex = List.len data0 |> Num.subWrap 1
    if dataIndexToRemoveU64 != lastDataIndex then
        # Swap removed item to the end
        data1 = List.swap data0 dataIndexToRemoveU64 lastDataIndex
        (key, _) = listGetUnsafe data1 dataIndexToRemoveU64

        # Update the data index of the new value.
        hash = hashKey key
        bucketIndex2 = bucketIndexFromHash hash shifts

        bucketIndex3 = scanForIndex buckets2 bucketIndex2 (Num.toU32 lastDataIndex)
        swapBucket = listGetUnsafe buckets2 bucketIndex3
        @Dict {
            buckets: List.set buckets2 bucketIndex3 { swapBucket & dataIndex: dataIndexToRemove },
            data: List.dropLast data1 1,
            maxBucketCapacity,
            maxLoadFactor,
            shifts,
        }
    else
        @Dict {
            buckets: buckets2,
            data: List.dropLast data0 1,
            maxBucketCapacity,
            maxLoadFactor,
            shifts,
        }

scanForIndex : List Bucket, U64, U32 -> U64
scanForIndex = \buckets, bucketIndex, dataIndex ->
    bucket = listGetUnsafe buckets bucketIndex
    if bucket.dataIndex != dataIndex then
        scanForIndex buckets (nextBucketIndex bucketIndex (List.len buckets)) dataIndex
    else
        bucketIndex

removeBucketHelper : List Bucket, U64 -> (List Bucket, U64)
removeBucketHelper = \buckets, bucketIndex ->
    nextIndex = nextBucketIndex bucketIndex (List.len buckets)
    nextBucket = listGetUnsafe buckets nextIndex
    # shift down until either empty or an element with correct spot is found
    if nextBucket.distAndFingerprint >= Num.mulWrap distInc 2 then
        List.set buckets bucketIndex { nextBucket & distAndFingerprint: decrementDist nextBucket.distAndFingerprint }
        |> removeBucketHelper nextIndex
    else
        (buckets, bucketIndex)

increaseSize : Dict k v -> Dict k v
increaseSize = \@Dict { data, maxBucketCapacity, maxLoadFactor, shifts } ->
    if maxBucketCapacity != maxBucketCount then
        newShifts = shifts |> Num.subWrap 1
        (buckets0, newMaxBucketCapacity) = allocBucketsFromShift newShifts maxLoadFactor
        buckets1 = fillBucketsFromData buckets0 data newShifts
        @Dict {
            buckets: buckets1,
            data,
            maxBucketCapacity: newMaxBucketCapacity,
            maxLoadFactor,
            shifts: newShifts,
        }
    else
        crash "Dict hit limit of $(Num.toStr maxBucketCount) elements. Unable to grow more."

allocBucketsFromShift : U8, F32 -> (List Bucket, U64)
allocBucketsFromShift = \shifts, maxLoadFactor ->
    bucketCount = calcNumBuckets shifts
    if bucketCount == maxBucketCount then
        # reached the maximum, make sure we can use each bucket
        (List.repeat emptyBucket maxBucketCount, maxBucketCount)
    else
        maxBucketCapacity =
            bucketCount
            |> Num.toF32
            |> Num.mul maxLoadFactor
            |> Num.floor
        (List.repeat emptyBucket bucketCount, maxBucketCapacity)

calcShiftsForSize : U64, F32 -> U8
calcShiftsForSize = \size, maxLoadFactor ->
    calcShiftsForSizeHelper initialShifts size maxLoadFactor

calcShiftsForSizeHelper = \shifts, size, maxLoadFactor ->
    maxBucketCapacity =
        shifts
        |> calcNumBuckets
        |> Num.toF32
        |> Num.mul maxLoadFactor
        |> Num.floor
    if shifts > 0 && maxBucketCapacity < size then
        calcShiftsForSizeHelper (shifts |> Num.subWrap 1) size maxLoadFactor
    else
        shifts

calcNumBuckets = \shifts ->
    Num.min
        (Num.shiftLeftBy 1 (64 |> Num.subWrap shifts))
        maxBucketCount

fillBucketsFromData = \buckets0, data, shifts ->
    List.walkWithIndex data buckets0 \buckets1, (key, _), dataIndex ->
        (bucketIndex, distAndFingerprint) = nextWhileLess buckets1 key shifts
        placeAndShiftUp buckets1 { distAndFingerprint, dataIndex: Num.toU32 dataIndex } bucketIndex

nextWhileLess : List Bucket, k, U8 -> (U64, U32) where k implements Hash & Eq
nextWhileLess = \buckets, key, shifts ->
    hash = hashKey key
    distAndFingerprint = distAndFingerprintFromHash hash
    bucketIndex = bucketIndexFromHash hash shifts

    nextWhileLessHelper buckets bucketIndex distAndFingerprint

nextWhileLessHelper = \buckets, bucketIndex, distAndFingerprint ->
    loaded = listGetUnsafe buckets bucketIndex
    if distAndFingerprint < loaded.distAndFingerprint then
        nextWhileLessHelper buckets (nextBucketIndex bucketIndex (List.len buckets)) (incrementDist distAndFingerprint)
    else
        (bucketIndex, distAndFingerprint)

placeAndShiftUp = \buckets0, bucket, bucketIndex ->
    loaded = listGetUnsafe buckets0 bucketIndex
    if loaded.distAndFingerprint != 0 then
        buckets1 = List.set buckets0 bucketIndex bucket
        placeAndShiftUp
            buckets1
            { loaded & distAndFingerprint: incrementDist loaded.distAndFingerprint }
            (nextBucketIndex bucketIndex (List.len buckets1))
    else
        List.set buckets0 bucketIndex bucket

nextBucketIndex = \bucketIndex, maxBuckets ->
    # I just ported this impl directly.
    # I am a bit confused why it is using an if over a mask.
    # Maybe compilers are smart enough to optimize this well.
    # Maybe the unlikely annotation is super important
    if Num.addWrap bucketIndex 1 != maxBuckets then
        Num.addWrap bucketIndex 1
    else
        0

hashKey = \key ->
    createLowLevelHasher PseudoRandSeed
    |> Hash.hash key
    |> complete

distAndFingerprintFromHash : U64 -> U32
distAndFingerprintFromHash = \hash ->
    hash
    |> Num.toU32
    |> Num.bitwiseAnd fingerprintMask
    |> Num.bitwiseOr distInc

bucketIndexFromHash : U64, U8 -> U64
bucketIndexFromHash = \hash, shifts ->
    hash
    |> Num.shiftRightZfBy shifts

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
        |> insert "h" 7
        |> insert "i" 8
        |> insert "j" 9
        |> insert "k" 10
        |> insert "l" 11
        |> capacity

    val == 12

# Reach capacity, all elements still exist
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
        |> insert "i" 8
        |> insert "j" 9
        |> insert "k" 10
        |> insert "l" 11

    (get dict "a" == Ok 0)
    && (get dict "b" == Ok 1)
    && (get dict "c" == Ok 2)
    && (get dict "d" == Ok 3)
    && (get dict "e" == Ok 4)
    && (get dict "f" == Ok 5)
    && (get dict "g" == Ok 6)
    && (get dict "h" == Ok 7)
    && (get dict "i" == Ok 8)
    && (get dict "j" == Ok 9)
    && (get dict "k" == Ok 10)
    && (get dict "l" == Ok 11)

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
        |> insert "i" 8
        |> insert "j" 9
        |> insert "k" 10
        |> insert "l" 11
        |> insert "m" 12
        |> capacity

    val == 25

# Force rehash, all elements still exist
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
        |> insert "i" 8
        |> insert "j" 9
        |> insert "k" 10
        |> insert "l" 11
        |> insert "m" 12

    (get dict "a" == Ok 0)
    && (get dict "b" == Ok 1)
    && (get dict "c" == Ok 2)
    && (get dict "d" == Ok 3)
    && (get dict "e" == Ok 4)
    && (get dict "f" == Ok 5)
    && (get dict "g" == Ok 6)
    && (get dict "h" == Ok 7)
    && (get dict "i" == Ok 8)
    && (get dict "j" == Ok 9)
    && (get dict "k" == Ok 10)
    && (get dict "l" == Ok 11)
    && (get dict "m" == Ok 12)

expect
    empty {}
    |> insert "Some" "Value"
    |> remove "Some"
    |> len
    |> Bool.isEq 0

# All BadKey's hash to the same location.
# This is needed to test some robinhood logic.
BadKey := U64 implements [
        Eq,
        Hash {
            hash: hashBadKey,
        },
    ]

hashBadKey : hasher, BadKey -> hasher where hasher implements Hasher
hashBadKey = \hasher, _ -> Hash.hash hasher 0

expect
    badKeys = [
        @BadKey 0,
        @BadKey 1,
        @BadKey 2,
        @BadKey 3,
        @BadKey 4,
        @BadKey 5,
        @BadKey 6,
        @BadKey 5,
        @BadKey 4,
        @BadKey 3,
        @BadKey 3,
        @BadKey 3,
        @BadKey 10,
    ]

    dict =
        List.walk badKeys (Dict.empty {}) \acc, k ->
            Dict.update acc k \val ->
                when val is
                    Ok p -> Ok (p |> Num.addWrap 1)
                    Err Missing -> Ok 0

    allInsertedCorrectly =
        List.walk badKeys Bool.true \acc, k ->
            acc && Dict.contains dict k

    allInsertedCorrectly

# Note, there are a number of places we should probably use set and replace unsafe.
# unsafe primitive that does not perform a bounds check
listGetUnsafe : List a, U64 -> a

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

    combineState (@LowLevelHasher { initializedSeed, state }) { a: abs.a, b: abs.b, seed: abs.seed, length }

hashBytesHelper48 : U64, U64, U64, List U8, U64, U64 -> { a : U64, b : U64, seed : U64 }
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

hashBytesHelper16 : U64, List U8, U64, U64 -> { a : U64, b : U64, seed : U64 }
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
    r = Num.mulWrap (Num.toU128 a) (Num.toU128 b)
    lower = Num.toU64 r
    upper = Num.shiftRightZfBy r 64 |> Num.toU64

    # This is the more robust form, which we may look into later
    # { lower: Num.bitwiseXor a lower, upper: Num.bitwiseXor b upper }
    { lower, upper }

# Get the next 8 bytes as a U64
wyr8 : List U8, U64 -> U64
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
wyr4 : List U8, U64 -> U64
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
wyr3 : List U8, U64, U64 -> U64
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

expect
    d1 =
        Dict.empty {}
        |> Dict.insert "Alice" 17
        |> Dict.insert "Bob" 18
        |> Dict.insert "Charlie" 19
        |> Dict.keepIf \(_k, v) -> v >= 18

    d2 =
        Dict.empty {}
        |> Dict.insert "Bob" 18
        |> Dict.insert "Charlie" 19

    d1 == d2

expect
    d1 =
        Dict.empty {}
        |> Dict.insert "Alice" 17
        |> Dict.insert "Bob" 18
        |> Dict.insert "Charlie" 19
        |> Dict.keepIf \(k, _v) -> Str.endsWith k "e"

    d2 =
        Dict.empty {}
        |> Dict.insert "Alice" 17
        |> Dict.insert "Charlie" 19

    d1 == d2

expect
    keysToDelete = [1, 2]
    d1 =
        Dict.empty {}
        |> Dict.insert 0 0
        |> Dict.insert 1 1
        |> Dict.insert 2 2
        |> Dict.insert 3 3
        |> Dict.insert 4 4
        |> Dict.keepIf (\(k, _v) -> List.contains keysToDelete k |> Bool.not)

    d2 =
        Dict.empty {}
        |> Dict.insert 0 0
        |> Dict.insert 3 3
        |> Dict.insert 4 4

    d1 == d2

expect
    keysToDelete = [2, 4]
    d1 =
        Dict.empty {}
        |> Dict.insert 0 0
        |> Dict.insert 1 1
        |> Dict.insert 2 2
        |> Dict.insert 3 3
        |> Dict.insert 4 4
        |> Dict.keepIf (\(k, _v) -> List.contains keysToDelete k |> Bool.not)

    d2 =
        Dict.empty {}
        |> Dict.insert 0 0
        |> Dict.insert 1 1
        |> Dict.insert 3 3

    d1 == d2
