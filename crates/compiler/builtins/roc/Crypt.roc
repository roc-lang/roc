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


Sha256 := {
    location : U64,
}

Digest256 := {
    firstHalf : U128,
    secondHalf : U128,
} implements[Eq]

emptySha256 : {} -> Sha256

sha256AddBytes : Sha256, List U8 -> Sha256

sha256Digest : Sha256 -> Digest256

hashSha256 : List U8 -> Digest256
hashSha256 = \bytes ->
    {} |> emptySha256
    |> sha256AddBytes bytes
    |> sha256Digest
    
u128Bytes : U128 -> List  U8
u128Bytes = \number ->
    loop = \ n, bytes, place ->
        if place == 16 then bytes
        else  
            newByte = n 
                |> Num.bitwiseAnd 255 
                |> Num.toU8
            loop (Num.shiftRightBy n 8) (List.prepend bytes newByte) (place + 1)
    loop number [] 0

digest256ToBytes : Digest256 -> List U8
digest256ToBytes = \@Digest256{firstHalf, secondHalf} ->
    List.concat (u128Bytes firstHalf) (u128Bytes secondHalf)
