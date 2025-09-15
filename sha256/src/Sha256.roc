##
# The `Sha256` module provides functionalities for computing SHA-256 hashes.
#
# Purpose:
# This module is designed to offer a straightforward way to generate SHA-256 hashes
# for various types of input, adhering to the FIPS 180-4 standard.
#
# Inputs and Outputs:
# The functions within this module primarily operate on `List U8` (list of bytes)
# and `Str` (string) types. Outputs are generally a 32-byte `List U8` for raw hash bytes
# or a 64-character `Str` for hexadecimal string representations of hashes.
#
# Exposed Functions:
# - `hash`: Computes the SHA-256 hash of a `List U8` and returns a `List U8`.
# - `hashToHex`: Computes the SHA-256 hash of a `List U8` and returns a hexadecimal `Str`.
# - `hashStrToHex`: Computes the SHA-256 hash of a `Str` and returns a hexadecimal `Str`.
# - `hashStrToBytes`: Computes the SHA-256 hash of a `Str` and returns a `List U8`.
##
module Sha256 exposing [Sha256.{ hash, hashToHex, hashStrToHex, hashStrToBytes }]

imports [
    Sha256.Internal.{ sha256Once }, # sha256Once : List U8 -> List U8
    Sha256.Internal.{ bytesToHex }, # bytesToHex : List U8 -> Str
    Roc.Utf8.{ toBytes }, # toBytes : Str -> List U8
]

# hash : List U8 -> List U8
#
# Purpose:
#   Computes the SHA-256 hash of a list of bytes.
#
# Parameters:
#   - `inputBytes` : `List U8` - The list of bytes to hash.
#
# Return Value:
#   - `List U8` - A 32-byte list representing the SHA-256 hash of `inputBytes`.
#
# Example:
#   ```roc
#   bytes : List U8 = [0x61, 0x62, 0x63] # "abc"
#   hashedBytes : List U8 = Sha256.hash bytes
#   # hashedBytes will be a 32-byte list representing the hash of "abc"
#   ```
hash : List U8 -> List U8
hash = \inputBytes ->
    # Call the internal sha256Once function which performs the SHA-256 algorithm
    sha256Once inputBytes

# hashToHex : List U8 -> Str
#
# Purpose:
#   Computes the SHA-256 hash of a list of bytes and returns it as a hexadecimal string.
#
# Parameters:
#   - `inputBytes` : `List U8` - The list of bytes to hash.
#
# Return Value:
#   - `Str` - A string representing the hexadecimal encoding of the SHA-256 hash.
#
# Example:
#   ```roc
#   bytes : List U8 = [0x61, 0x62, 0x63] # "abc"
#   hexStr : Str = Sha256.hashToHex bytes
#   # hexStr will be "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
#   ```
hashToHex : List U8 -> Str
hashToHex = \inputBytes ->
    hashedBytes = hash inputBytes
    bytesToHex hashedBytes

# hashStrToHex : Str -> Str
#
# Purpose:
#   Computes the SHA-256 hash of a string and returns it as a hexadecimal string.
#
# Parameters:
#   - `inputStr` : `Str` - The string to hash.
#
# Return Value:
#   - `Str` - A string representing the hexadecimal encoding of the SHA-256 hash.
#
# Assumptions:
#   - The input string `inputStr` is assumed to be UTF-8 encoded.
#     The function uses `Roc.Utf8.toBytes` for conversion.
#
# Example:
#   ```roc
#   text : Str = "hello world"
#   hexHash : Str = Sha256.hashStrToHex text
#   # hexHash will be "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
#   ```
hashStrToHex : Str -> Str
hashStrToHex = \inputStr ->
    inputBytes = Roc.Utf8.toBytes inputStr
    hashToHex inputBytes

# hashStrToBytes : Str -> List U8
#
# Purpose:
#   Computes the SHA-256 hash of a string and returns it as a list of bytes.
#
# Parameters:
#   - `inputStr` : `Str` - The string to hash.
#
# Return Value:
#   - `List U8` - A 32-byte list representing the SHA-256 hash of the UTF-8
#     encoded `inputStr`.
#
# Assumptions:
#   - The input string `inputStr` is assumed to be UTF-8 encoded.
#     The function uses `Roc.Utf8.toBytes` for conversion.
#
# Example:
#   ```roc
#   text : Str = "Roc rocks!"
#   byteHash : List U8 = Sha256.hashStrToBytes text
#   # byteHash will be a 32-byte list representing the hash of "Roc rocks!"
#   ```
hashStrToBytes : Str -> List U8
hashStrToBytes = \inputStr ->
    inputBytes = Roc.Utf8.toBytes inputStr
    hash inputBytes
