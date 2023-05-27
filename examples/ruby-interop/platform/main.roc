platform "ruby-interop"
    requires {} { main : arg -> ret | arg implements Decoding, ret implements Encoding }
    exposes []
    packages {}
    imports [Json]
    provides [mainForHost]

mainForHost : List U8 -> List U8
mainForHost = \json ->
    when Decode.fromBytes json Json.json is
        Ok arg -> Encode.toBytes (main arg) Json.json
        Err _ -> [] # TODO panic so that Ruby raises an exception
