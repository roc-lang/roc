module [
    emptySha256,
    addBytes,
    digest,
    hashSha256,
    digest256ToBytes,
    Sha256,
    Digest256,
]

import Bool exposing [Eq]
import List
import Num exposing [U8, U64, U128]


Sha256 := {
    location : U64,
}

Digest256 := {
    firstHalf : U128,
    secondHalf : U128,
} implements[Eq]

emptySha256 : {} -> Sha256

addBytes : Sha256, List U8 -> Sha256

digest : Sha256 -> Digest256

hashSha256 : List U8 -> Digest256
hashSha256 = \bytes ->
    digest (addBytes (emptySha256 {}) bytes)
    
u128Bytes : U128 -> List  U8
u128Bytes = \ number ->
    loop = \ n, bytes, place ->
        if place == 16 then bytes
        else  
            newByte = n |> Num.bitwiseAnd 255 |> Num.toU8 
            loop (Num.shiftRightBy n 8) (List.prepend bytes newByte) (place + 1)
    loop number [] 0

digest256ToBytes : Digest256 -> List U8
digest256ToBytes \ digest256 -> 
    List.append (u128Bytes digest256.firstHalf) (u128Bytes digest256.secondHalf)
