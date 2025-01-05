module [from_bytes, from_str, to_bytes, to_str]

import Base64.Decode
import Base64.Encode

# base 64 encoding from a sequence of bytes
from_bytes : List U8 -> Result Str [InvalidInput]
from_bytes = \bytes ->
    when Base64.Decode.from_bytes(bytes) is
        Ok(v) ->
            Ok(v)

        Err(_) ->
            Err(InvalidInput)

# base 64 encoding from a string
from_str : Str -> Result Str [InvalidInput]
from_str = \str ->
    from_bytes(Str.toUtf8(str))

# base64-encode bytes to the original
to_bytes : Str -> Result (List U8) [InvalidInput]
to_bytes = \str ->
    Ok(Base64.Encode.to_bytes(str))

to_str : Str -> Result Str [InvalidInput]
to_str = \str ->
    when to_bytes(str) is
        Ok(bytes) ->
            when Str.fromUtf8(bytes) is
                Ok(v) ->
                    Ok(v)

                Err(_) ->
                    Err(InvalidInput)

        Err(_) ->
            Err(InvalidInput)
