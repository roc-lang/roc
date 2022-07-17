app "helloWorld"
    imports [Encode.{ toEncoder }, Json]
    provides [main] to "./platform"

HelloWorld := {}

toEncoder = \@HelloWorld {} ->
    Encode.custom \bytes, fmt ->
        bytes
            |> Encode.appendWith (Encode.string "Hello, World!\n") fmt

result = Str.fromUtf8 (Encode.toBytes (@HelloWorld {}) Json.format)
when result is
    Ok s -> s
    _ -> "<bad>"
