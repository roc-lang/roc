module [
    Hash,
    Hasher,
    hash,
    add_bytes,
    add_u8,
    add_u16,
    add_u32,
    add_u64,
    add_u128,
    hash_bool,
    hash_i8,
    hash_i16,
    hash_i32,
    hash_i64,
    hash_i128,
    hash_dec,
    complete,
    hash_str_bytes,
    hash_list,
    hash_unordered,
]

import Bool exposing [Bool]
import List
import Str
import Num exposing [
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    Dec,
]

## A value that can be hashed.
Hash implements
    ## Hashes a value into a [Hasher].
    ## Note that [hash] does not produce a hash value itself; the hasher must be
    ## [complete]d in order to extract the hash value.
    hash : hasher, a -> hasher where a implements Hash, hasher implements Hasher

## Describes a hashing algorithm that is fed bytes and produces an integer hash.
##
## The [Hasher] ability describes general-purpose hashers. It only allows
## emission of 64-bit unsigned integer hashes. It is not suitable for
## cryptographically-secure hashing.
Hasher implements
    ## Adds a list of bytes to the hasher.
    add_bytes : a, List U8 -> a where a implements Hasher

    ## Adds a single U8 to the hasher.
    add_u8 : a, U8 -> a where a implements Hasher

    ## Adds a single U16 to the hasher.
    add_u16 : a, U16 -> a where a implements Hasher

    ## Adds a single U32 to the hasher.
    add_u32 : a, U32 -> a where a implements Hasher

    ## Adds a single U64 to the hasher.
    add_u64 : a, U64 -> a where a implements Hasher

    ## Adds a single U128 to the hasher.
    add_u128 : a, U128 -> a where a implements Hasher

    ## Completes the hasher, extracting a hash value from its
    ## accumulated hash state.
    complete : a -> U64 where a implements Hasher

## Adds a string into a [Hasher] by hashing its UTF-8 bytes.
hash_str_bytes = \hasher, s ->
    add_bytes(hasher, Str.to_utf8(s))

## Adds a list of [Hash]able elements to a [Hasher] by hashing each element.
hash_list = \hasher, lst ->
    List.walk(
        lst,
        hasher,
        \accum_hasher, elem ->
            hash(accum_hasher, elem),
    )

## Adds a single [Bool] to a hasher.
hash_bool : a, Bool -> a where a implements Hasher
hash_bool = \hasher, b ->
    as_u8 = if b then 1 else 0
    add_u8(hasher, as_u8)

## Adds a single I8 to a hasher.
hash_i8 : a, I8 -> a where a implements Hasher
hash_i8 = \hasher, n -> add_u8(hasher, Num.to_u8(n))

## Adds a single I16 to a hasher.
hash_i16 : a, I16 -> a where a implements Hasher
hash_i16 = \hasher, n -> add_u16(hasher, Num.to_u16(n))

## Adds a single I32 to a hasher.
hash_i32 : a, I32 -> a where a implements Hasher
hash_i32 = \hasher, n -> add_u32(hasher, Num.to_u32(n))

## Adds a single I64 to a hasher.
hash_i64 : a, I64 -> a where a implements Hasher
hash_i64 = \hasher, n -> add_u64(hasher, Num.to_u64(n))

## Adds a single I128 to a hasher.
hash_i128 : a, I128 -> a where a implements Hasher
hash_i128 = \hasher, n -> add_u128(hasher, Num.to_u128(n))

## Adds a single [Dec] to a hasher.
hash_dec : a, Dec -> a where a implements Hasher
hash_dec = \hasher, n -> hash_i128(hasher, Num.without_decimal_point(n))

## Adds a container of [Hash]able elements to a [Hasher] by hashing each element.
## The container is iterated using the walk method passed in.
## The order of the elements does not affect the final hash.
hash_unordered = \hasher, container, walk ->
    walk(
        container,
        0,
        \accum, elem ->
            x =
                # Note, we intentionally copy the hasher in every iteration.
                # Having the same base state is required for unordered hashing.
                hasher
                |> hash(elem)
                |> complete
            next_accum = Num.add_wrap(accum, x)

            if next_accum < accum then
                # we don't want to lose a bit of entropy on overflow, so add it back in.
                Num.add_wrap(next_accum, 1)
            else
                next_accum,
    )
    |> \accum -> add_u64(hasher, accum)
