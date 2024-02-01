interface Base64 exposes [fromBytes, fromStr, toBytes, toStr]

import Base64Decode

# base 64 encoding from a sequence of bytes
fromBytes : List U8 -> Result Str [InvalidInput]
fromBytes = \bytes ->
    when Base64Decode.fromBytes bytes is
        Ok v ->
            Ok v

        Err _ ->
            Err InvalidInput

# base 64 encoding from a string
fromStr : Str -> Result Str [InvalidInput]
fromStr = \str ->
    fromBytes (Str.toUtf8 str)

# base64-encode bytes to the original
toBytes : Str -> Result (List U8) [InvalidInput]
toBytes = \str ->
    Ok (Base64Decode.toBytes str)

toStr : Str -> Result Str [InvalidInput]
toStr = \str ->
    when toBytes str is
        Ok bytes ->
            when Str.fromUtf8 bytes is
                Ok v ->
                    Ok v

                Err _ ->
                    Err InvalidInput

        Err _ ->
            Err InvalidInput
