module [
    emptySha256,
    addBytes,
    digest,
    hashSha256,
    digest256Eq,
    digest256ByteList,
    Sha256,
    Digest256,
]

import List
import Num exposing [U8, U64, U128]

Sha256 := {
    location : U64,
}

Digest256 := {
    firstHalf : U128,
    secondHalf : U128,
}

emptySha256 : {} -> Sha256

addBytes : Sha256, List.List U8 -> Sha256

digest : Sha256 -> Digest256

digest256Eq : Digest256, Digest256 -> Bool.Bool

digest256ByteList: Digest256 -> List.List U8

hashSha256 : List U8 -> Digest256
hashSha256 = \bytes ->
    digest (addBytes (emptySha256 {}) bytes)
