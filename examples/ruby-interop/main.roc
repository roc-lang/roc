app "libhello"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main : List U8 -> List U8
main = \json ->
    if List.isEmpty json then
        "I need some JSON here!" |> Str.toUtf8
    else
        str = Str.fromUtf8 json |> Result.withDefault "Invalid UTF-8 in JSON from Ruby"
        "\(str), OH YEAH!!! 🤘🤘" |> Str.toUtf8
