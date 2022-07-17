app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports [Json, Encode]
    provides [main] to pf

main =
    toBytes "blah" Json.format
    |> .val
    |> Str.fromUtf8
    |> Result.withDefault ""

toBytes : {}
toBytes = \path, val, fmt ->
    { thing: path, val: Encode.toBytes val fmt }
