platform "jvm-interop"
    requires {} { main : arg -> ret | arg has Decoding }
    exposes []
    packages {}
    imports [Json]
    provides [mainForHost]

mainForHost : List U8 -> Str
mainForHost = \json ->
    when Decode.fromBytes json Json.fromUtf8 is
        Ok arg -> main arg
        Err _ -> "Decoding Error"
