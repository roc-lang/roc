module[
    emptySha256, 
    addBytes, 
    digest, 
    hashSha256,
]
import List
emptySha256 : {} -> Sha256

addBytes : Sha256, List.List u8 -> Sha256

digest : Sha256 -> Digest256

hashSha256 : List u8 -> Digest256
hashSha256 = \bytes -> 
   digest ( addBytes (emptySha256 {}) bytes)
