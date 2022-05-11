app "encode"
    packages { pf: "rust-platform" }
    imports [ Encode.{ toEncoder }, Json ]
    provides [ main ] to pf

HelloWorld := {}

toEncoder = \@HelloWorld {} ->
    Encode.custom \bytes, fmt ->
        bytes
        |> Encode.appendWith (Encode.string "{") fmt
        |> Encode.appendWith (Encode.string "Hello, World!\n") fmt
        |> Encode.appendWith (Encode.string "}") fmt

main =
    when Str.fromUtf8 (Encode.toBytes (@HelloWorld {}) Json.format) is
        Ok s -> s
        _ -> "<bad>"
