module [
    emptySha256,
    sha256AddBytes,
    sha256Digest,
    hashSha256,
    digest256ToBytes,
    Sha256,
    Digest256,
]

import Bool exposing [Eq]
import List
import Num exposing [U8, U64, U128]
import Result
import Str

## Represents the state of a SHA-256 cryptographic hashing function, after some (or no) data has been added to the hash.
Sha256 := { location : U64 }

## Represents the digest of some data produced by the SHA-256 cryptographic hashing function as an opaque type.

## `Digest256` implements the `Eq` ability.
Digest256 := { firstHalf : U128, secondHalf : U128 } implements [Eq]

## Returns an empty SHA-256 hasher.
emptySha256 : {} -> Sha256

## Adds bytes of data to be hashed by a SHA-256 hasher..
sha256AddBytes : Sha256, List U8 -> Sha256

## Returns the digest of the cryptographic hashing function represented by a SHA-256 hasher..
sha256Digest : Sha256 -> Digest256

## Applies the SHA-256 crytographic hashing function to some bytes.
hashSha256 : List U8 -> Digest256
hashSha256 = \bytes -> emptySha256 {} |> sha256AddBytes bytes |> sha256Digest

u128Bytes : U128 -> List U8
u128Bytes = \number ->
    loop = \n, bytes, place ->
        if place == 16 then
            bytes
        else
            newByte = n |> Num.bitwiseAnd 255 |> Num.toU8
            loop (Num.shiftRightBy n 8) (List.prepend bytes newByte) (place + 1)
    loop number [] 0

## Returns the bytes of a SHA-256 digest as a list.
digest256ToBytes : Digest256 -> List U8
digest256ToBytes = \@Digest256 { firstHalf, secondHalf } ->
    List.concat (u128Bytes firstHalf) (u128Bytes secondHalf)

# test data taken from https://ziglang.org/documentation/0.11.0/std/src/std/crypto/sha2.zig.html#L434
digestBytesOfEmpty : List U8
digestBytesOfEmpty = fromHexString "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

digestBytesOfAbc : List U8
digestBytesOfAbc = fromHexString "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

digestBytesOfLong : List U8
digestBytesOfLong = fromHexString "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"

expect
    data : List U8
    data = []
    want = digestBytesOfEmpty
    got = data |> hashSha256 |> digest256ToBytes
    want == got

expect
    data = ['a', 'b', 'c']
    want = digestBytesOfAbc
    got = data |> hashSha256 |> digest256ToBytes
    want == got

expect
    data = Str.toUtf8 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
    want = digestBytesOfLong
    got = data |> hashSha256 |> digest256ToBytes
    want == got

expect
    want = digestBytesOfEmpty
    got = emptySha256 {} |> sha256Digest |> digest256ToBytes
    want == got

expect
    data = ['a', 'b', 'c']
    want = digestBytesOfAbc
    got =
        emptySha256 {}
        |> sha256AddBytes data
        |> sha256Digest
        |> digest256ToBytes
    want == got

expect
    want = digestBytesOfAbc
    got =
        emptySha256 {}
        |> sha256AddBytes ['a']
        |> sha256AddBytes ['b']
        |> sha256AddBytes ['c']
        |> sha256Digest
        |> digest256ToBytes
    want == got

fromHexString : Str -> List U8
fromHexString = \hex ->
    fromHexDigit = \smallNumber ->
        if smallNumber <= '9' then
            smallNumber - '0'
        else
            smallNumber - 'a' + 10

    fromHexDigits = \pair ->
        first = pair |> List.first |> Result.withDefault 0
        second = pair |> List.get 1 |> Result.withDefault 0
        16 * (fromHexDigit first) + (fromHexDigit second)

    hex
    |> Str.toUtf8
    |> List.chunksOf 2
    |> List.map fromHexDigits
