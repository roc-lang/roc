interface Dict
    exposes [
        Dict,
        empty,
        withCapacity,
        single,
        get,
        walk,
        insert,
        len,
        remove,
        update,
        contains,
        keys,
        values,
        insertAll,
        keepShared,
        removeAll,
    ]
    imports [
        Bool.{ Bool, Eq },
        Result.{ Result },
        List,
        Str,
        Num.{ Nat, U64, U8 },
        Hash.{ Hasher },
    ]

## A [dictionary](https://en.wikipedia.org/wiki/Associative_array) that lets you can associate keys with values.
##
## ### Inserting
##
## The most basic way to use a dictionary is to start with an empty one and then:
## 1. Call [Dict.insert] passing a key and a value, to associate that key with that value in the dictionary.
## 2. Later, call [Dict.get] passing the same key as before, and it will return the value you stored.
##
## Here's an example of a dictionary which uses a city's name as the key, and its population as the associated value.
##
##     populationByCity =
##         Dict.empty
##         |> Dict.insert "London" 8_961_989
##         |> Dict.insert "Philadelphia" 1_603_797
##         |> Dict.insert "Shanghai" 24_870_895
##         |> Dict.insert "Delhi" 16_787_941
##         |> Dict.insert "Amsterdam" 872_680
##
## ### Accessing keys or values
##
## We can use [Dict.keys] and [Dict.values] functions to get only the keys or only the values.
##
## You may notice that these lists have the same order as the original insertion order. This will be true if
## all you ever do is [insert] and [get] operations on the dictionary, but [remove] operations can change this order.
## Let's see how that looks.
##
## ### Removing
##
## We can remove an element from the dictionary, like so:
##
##     populationByCity
##         |> Dict.remove "Philadelphia"
##         |> Dict.keys
##         ==
##         ["London", "Amsterdam", "Shanghai", "Delhi"]
##
## Notice that the order changed! Philadelphia has been not only removed from the list, but Amsterdam - the last
## entry we inserted - has been moved into the spot where Philadelphia was previously. This is exactly what
## [Dict.remove] does: it removes an element and moves the most recent insertion into the vacated spot.
##
## This move is done as a performance optimization, and it lets [remove] have
## [constant time complexity](https://en.wikipedia.org/wiki/Time_complexity#Constant_time). ##
##
## ### Equality
##
## When comparing two dictionaries for equality, they are `==` only if their both their contents and their
## orderings match. This preserves the property that if `dict1 == dict2`, you should be able to rely on
## `fn dict1 == fn dict2` also being `Bool.true`, even if `fn` relies on the dictionary's ordering.
Dict k v := List [Pair k v] has [Eq]

## An empty dictionary.
empty : Dict k v
empty = @Dict []

withCapacity : Nat -> Dict k v
withCapacity = \n -> @Dict (List.withCapacity n)

get : Dict k v, k -> Result v [KeyNotFound]* | k has Eq
get = \@Dict list, needle ->
    when List.findFirst list (\Pair key _ -> key == needle) is
        Ok (Pair _ v) ->
            Ok v

        Err NotFound ->
            Err KeyNotFound

walk : Dict k v, state, (state, k, v -> state) -> state
walk = \@Dict list, initialState, transform ->
    List.walk list initialState (\state, Pair k v -> transform state k v)

insert : Dict k v, k, v -> Dict k v | k has Eq
insert = \@Dict list, k, v ->
    when List.findFirstIndex list (\Pair key _ -> key == k) is
        Err NotFound ->
            insertFresh (@Dict list) k v

        Ok index ->
            list
            |> List.set index (Pair k v)
            |> @Dict

len : Dict k v -> Nat
len = \@Dict list ->
    List.len list

remove : Dict k v, k -> Dict k v | k has Eq
remove = \@Dict list, key ->
    when List.findFirstIndex list (\Pair k _ -> k == key) is
        Err NotFound ->
            @Dict list

        Ok index ->
            lastIndex = List.len list - 1

            list
            |> List.swap index lastIndex
            |> List.dropLast
            |> @Dict

## Insert or remove a value in a Dict based on its presence
update : Dict k v, k, ([Present v, Missing] -> [Present v, Missing]) -> Dict k v | k has Eq
update = \dict, key, alter ->
    possibleValue =
        get dict key
        |> Result.map Present
        |> Result.withDefault Missing

    when alter possibleValue is
        Present value -> insert dict key value
        Missing -> remove dict key

## Internal for testing only
alterValue : [Present Bool, Missing] -> [Present Bool, Missing]
alterValue = \possibleValue ->
    when possibleValue is
        Missing -> Present Bool.false
        Present value if Bool.not value -> Present Bool.true
        Present _ -> Missing

expect update empty "a" alterValue == single "a" Bool.false
expect update (single "a" Bool.false) "a" alterValue == single "a" Bool.true
expect update (single "a" Bool.true) "a" alterValue == empty

contains : Dict k v, k -> Bool | k has Eq
contains = \@Dict list, needle ->
    step = \_, Pair key _val ->
        if key == needle then
            Break {}
        else
            Continue {}

    when List.iterate list {} step is
        Continue _ -> Bool.false
        Break _ -> Bool.true

single : k, v -> Dict k v
single = \key, value ->
    @Dict [Pair key value]

## Returns a [List] of the dictionary's keys.
keys : Dict k v -> List k
keys = \@Dict list ->
    List.map list (\Pair k _ -> k)

## Returns a [List] of the Dict's values
values : Dict k v -> List v
values = \@Dict list ->
    List.map list (\Pair _ v -> v)

# union : Dict k v, Dict k v -> Dict k v
insertAll : Dict k v, Dict k v -> Dict k v | k has Eq
insertAll = \xs, @Dict ys ->
    List.walk ys xs (\state, Pair k v -> Dict.insertIfVacant state k v)

# intersection : Dict k v, Dict k v -> Dict k v
keepShared : Dict k v, Dict k v -> Dict k v | k has Eq
keepShared = \@Dict xs, ys ->
    List.keepIf xs (\Pair k _ -> Dict.contains ys k)
    |> @Dict

# difference : Dict k v, Dict k v -> Dict k v
removeAll : Dict k v, Dict k v -> Dict k v | k has Eq
removeAll = \xs, @Dict ys ->
    List.walk ys xs (\state, Pair k _ -> Dict.remove state k)

## Internal helper function to insert a new association
##
## Precondition: `k` should not exist in the Dict yet.
insertFresh : Dict k v, k, v -> Dict k v
insertFresh = \@Dict list, k, v ->
    list
    |> List.append (Pair k v)
    |> @Dict

insertIfVacant : Dict k v, k, v -> Dict k v | k has Eq
insertIfVacant = \dict, key, value ->
    if Dict.contains dict key then
        dict
    else
        Dict.insert dict key value

# We have decided not to expose the standard roc hashing algorithm.
# This is to avoid external dependence and the need for versioning.
# The current implementation is a form of [Wyhash final3](https://github.com/wangyi-fudan/wyhash/blob/a5995b98ebfa7bd38bfadc0919326d2e7aabb805/wyhash.h).
# It is 64bit and little endian specific currently.
# TODO: wyhash is slow for large keys, use something like cityhash if the keys are too long.
# TODO: Add a builtin to distinguish big endian systems and change loading orders.
# TODO: Switch out Wymum on systems with slow 128bit multiplication.
LowLevelHasher := { originalSeed : U64, state : U64 } has [
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

createLowLevelHasher : { seed ?U64 } -> LowLevelHasher
createLowLevelHasher = \{ seed ? 0x526F_6352_616E_643F } ->
    @LowLevelHasher { originalSeed: seed, state: seed }

combineState : LowLevelHasher, { a : U64, b : U64, seed : U64, length : U64 } -> LowLevelHasher
combineState = \@LowLevelHasher { originalSeed, state }, { a, b, seed, length } ->
    tmp = wymix (Num.bitwiseXor wyp1 a) (Num.bitwiseXor seed b)
    hash = wymix (Num.bitwiseXor wyp1 length) tmp

    @LowLevelHasher { originalSeed, state: wymix state hash }

complete = \@LowLevelHasher { state } -> state

# These implementations hash each value individually with the seed and then mix
# the resulting hash with the state. There are other options that may be faster
# like using the output of the last hash as the seed to the current hash.
# I am simply not sure the tradeoffs here. Theoretically this method is more sound.
# Either way, the performance will be similar and we can change this later.
addU8 = \@LowLevelHasher { originalSeed, state }, u8 ->
    seed = Num.bitwiseXor originalSeed wyp0
    p0 = Num.toU64 u8
    a =
        Num.shiftLeftBy p0 16
        |> Num.bitwiseOr (Num.shiftLeftBy p0 8)
        |> Num.bitwiseOr p0
    b = 0

    combineState (@LowLevelHasher { originalSeed, state }) { a, b, seed, length: 1 }

addU16 = \@LowLevelHasher { originalSeed, state }, u16 ->
    seed = Num.bitwiseXor originalSeed wyp0
    p0 = Num.bitwiseAnd u16 0xFF |> Num.toU64
    p1 = Num.shiftRightZfBy u16 8 |> Num.toU64
    a =
        Num.shiftLeftBy p0 16
        |> Num.bitwiseOr (Num.shiftLeftBy p1 8)
        |> Num.bitwiseOr p1
    b = 0

    combineState (@LowLevelHasher { originalSeed, state }) { a, b, seed, length: 2 }

addU32 = \@LowLevelHasher { originalSeed, state }, u32 ->
    seed = Num.bitwiseXor originalSeed wyp0
    p0 = Num.toU64 u32
    a = Num.shiftLeftBy p0 32 |> Num.bitwiseOr p0

    combineState (@LowLevelHasher { originalSeed, state }) { a, b: a, seed, length: 4 }

addU64 = \@LowLevelHasher { originalSeed, state }, u64 ->
    seed = Num.bitwiseXor originalSeed wyp0
    p0 = Num.bitwiseAnd 0xFFFF_FFFF u64
    p1 = Num.shiftRightZfBy u64 32
    a = Num.shiftLeftBy p0 32 |> Num.bitwiseOr p1
    b = Num.shiftLeftBy p1 32 |> Num.bitwiseOr p0

    combineState (@LowLevelHasher { originalSeed, state }) { a, b, seed, length: 8 }

addU128 = \@LowLevelHasher { originalSeed, state }, u128 ->
    seed = Num.bitwiseXor originalSeed wyp0
    lower = u128 |> Num.toU64
    upper = Num.shiftRightZfBy u128 64 |> Num.toU64
    p0 = Num.bitwiseAnd 0xFFFF_FFFF lower
    p1 = Num.shiftRightZfBy lower 32 |> Num.bitwiseAnd 0xFFFF_FFFF
    p2 = Num.bitwiseAnd 0xFFFF_FFFF upper
    p3 = Num.shiftRightZfBy upper 32 |> Num.bitwiseAnd 0xFFFF_FFFF
    a = Num.shiftLeftBy p0 32 |> Num.bitwiseOr p2
    b = Num.shiftLeftBy p3 32 |> Num.bitwiseOr p1

    combineState (@LowLevelHasher { originalSeed, state }) { a, b, seed, length: 16 }

addBytes : LowLevelHasher, List U8 -> LowLevelHasher
addBytes = \@LowLevelHasher { originalSeed, state }, list ->
    length = List.len list
    seed = Num.bitwiseXor originalSeed wyp0
    abs =
        if length <= 16 then
            if length >= 4 then
                x = Num.shiftRightZfBy length 3 |> Num.shiftLeftBy 2
                a = Num.bitwiseOr (wyr4 list 0 |> Num.shiftLeftBy 32) (wyr4 list x)
                b =
                    (wyr4 list (Num.subWrap length 4) |> Num.shiftLeftBy 32)
                    |> Num.bitwiseOr (wyr4 list (Num.subWrap length 4 |> Num.subWrap x))

                { a, b, seed }
            else if length > 0 then
                { a: wyr3 list 0 length, b: 0, seed }
            else
                { a: 0, b: 0, seed }
        else if length <= 48 then
            hashBytesHelper16 seed list 0 length
        else
            hashBytesHelper48 seed seed seed list 0 length

    combineState (@LowLevelHasher { originalSeed, state }) { a: abs.a, b: abs.b, seed: abs.seed, length: Num.toU64 length }

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

    # This is the more robust form.
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

# TODO: would be great to have table driven expects for this.
# Would also be great to have some sort of property based hasher
# where we can compare `addU*` functions to the `addBytes` function.
expect
    hash =
        createLowLevelHasher {}
        |> addBytes []
        |> complete

    hash == 0x1C3F_F8BF_07F9_B0B3

expect
    hash =
        createLowLevelHasher {}
        |> addBytes [0x42]
        |> complete

    hash == 0x8F9F_0A1E_E06F_0D52

expect
    hash =
        createLowLevelHasher {}
        |> addU8 0x42
        |> complete

    hash == 0x8F9F_0A1E_E06F_0D52

expect
    hash =
        createLowLevelHasher {}
        |> addBytes [0xFF, 0xFF]
        |> complete

    hash == 0x86CC_8B71_563F_F084

expect
    hash =
        createLowLevelHasher {}
        |> addU16 0xFFFF
        |> complete

    hash == 0x86CC_8B71_563F_F084

expect
    hash =
        createLowLevelHasher {}
        |> addBytes [0x36, 0xA7]
        |> complete

    hash == 0xD1A5_0F24_2536_84F8

expect
    hash =
        createLowLevelHasher {}
        |> addU16 0xA736
        |> complete

    hash == 0xD1A5_0F24_2536_84F8

expect
    hash =
        createLowLevelHasher {}
        |> addBytes [0x00, 0x00, 0x00, 0x00]
        |> complete

    hash == 0x3762_ACB1_7604_B541

expect
    hash =
        createLowLevelHasher {}
        |> addU32 0x0000_0000
        |> complete

    hash == 0x3762_ACB1_7604_B541

expect
    hash =
        createLowLevelHasher {}
        |> addBytes [0xA9, 0x2F, 0xEE, 0x21]
        |> complete

    hash == 0x20F3_3FD7_D32E_C7A9

expect
    hash =
        createLowLevelHasher {}
        |> addU32 0x21EE_2FA9
        |> complete

    hash == 0x20F3_3FD7_D32E_C7A9

expect
    hash =
        createLowLevelHasher {}
        |> addBytes [0x5D, 0x66, 0xB1, 0x8F, 0x68, 0x44, 0xC7, 0x03, 0xE1, 0xDD, 0x23, 0x34, 0xBB, 0x9A, 0x42, 0xA7]
        |> complete

    hash == 0xA16F_DDAA_C167_74C7

expect
    hash =
        createLowLevelHasher {}
        |> addU128 0xA742_9ABB_3423_DDE1_03C7_4468_8FB1_665D
        |> complete

    hash == 0xA16F_DDAA_C167_74C7

expect
    hash =
        createLowLevelHasher {}
        |> Hash.hashStrBytes "abcdefghijklmnopqrstuvwxyz"
        |> complete

    hash == 0xBEE0_A8FD_E990_D285

expect
    hash =
        createLowLevelHasher {}
        |> Hash.hashStrBytes "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        |> complete

    hash == 0xB3C5_8528_9D82_A6EF

expect
    hash =
        createLowLevelHasher {}
        |> Hash.hashStrBytes "1234567890123456789012345678901234567890123456789012345678901234567890"
        |> complete

    hash == 0xDB6B_7997_7A55_BA03

expect
    hash =
        createLowLevelHasher {}
        |> addBytes (List.repeat 0x77 100)
        |> complete

    hash == 0x171F_EEE2_B764_8E5E

# Note, had to specify u8 in the lists below to avoid ability type resolution error.
# Apparently it won't pick the default integer.
expect
    hash =
        createLowLevelHasher {}
        |> Hash.hashUnordered [8u8, 82u8, 3u8, 8u8, 24u8] List.walk
        |> complete

    hash == 0x999F_B530_3529_F17D

expect
    hash1 =
        createLowLevelHasher {}
        |> Hash.hashUnordered ([0u8, 1u8, 2u8, 3u8, 4u8]) List.walk
        |> complete

    hash2 =
        createLowLevelHasher {}
        |> Hash.hashUnordered [4u8, 3u8, 2u8, 1u8, 0u8] List.walk
        |> complete

    hash1 == hash2

expect
    hash1 =
        createLowLevelHasher {}
        |> Hash.hashUnordered [0u8, 1u8, 2u8, 3u8, 4u8] List.walk
        |> complete

    hash2 =
        createLowLevelHasher {}
        |> Hash.hashUnordered [4u8, 3u8, 2u8, 1u8, 0u8, 0u8] List.walk
        |> complete

    hash1 != hash2
