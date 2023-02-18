platform "jvm-interop"
    requires {} { main : arg -> ret | arg has Decoding, ret has Encoding }
    exposes []
    packages {}
    imports [Json]
    provides [mainForHost]

mainForHost : List U8 -> List U8
mainForHost = \json ->
    when Decode.fromBytes json Json.fromUtf8 is
        Ok arg -> Encode.toBytes (main arg) Json.toUtf8
        Err _ -> []
