app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports [Inter, Decode, Json]
    provides [main] to pf

main =
    when Str.toUtf8 "{\"first\":\"ab\",\"second\":[\"cd\",\"ef\"]}" |> Decode.decodeWith Inter.theDecoder Json.fromUtf8 is
        {result, rest: _} ->
            when result is
                Ok { first, second } -> Str.concat first (Str.joinWith second ",")
                Err _ -> "<bad>"
