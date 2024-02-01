platform "ruby-interop"
    requires {} { main : arg -> ret where arg implements Decoding, ret implements Encoding }
    exposes []
    packages {}
    provides [mainForHost]

import TotallyNotJson

mainForHost : List U8 -> List U8
mainForHost = \json ->
    when Decode.fromBytes json TotallyNotJson.json is
        Ok arg -> Encode.toBytes (main arg) TotallyNotJson.json
        Err _ -> [] # TODO panic so that Ruby raises an exception
