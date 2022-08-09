app "helloWorld"
    packages { pf: "platform/main.roc" }
    imports [Decode, Decode.{Decoder, Decoding, DecoderFormatting}, Json]
    provides [main] to pf

theDecoder : Decoder {first: a, second: b} fmt | a has Decoding, b has Decoding, fmt has DecoderFormatting
theDecoder =
    initialState : {f0: Result a [NoField], f1: Result b [NoField]}
    initialState = {f0: Err NoField, f1: Err NoField}

    stepField = \state, field ->
        when field is
            "first" ->
                Keep (Decode.custom \bytes, fmt ->
                    when Decode.decodeWith bytes Decode.decoder fmt is
                        {result, rest} ->
                            {result: Result.map result \val -> {state & f0: Ok val}, rest})
            "second" ->
                Keep (Decode.custom \bytes, fmt ->
                    when Decode.decodeWith bytes Decode.decoder fmt is
                        {result, rest} ->
                            {result: Result.map result \val -> {state & f1: Ok val}, rest})
            _ -> Skip

    finalizer = \{f0, f1} ->
        when f0 is
            Ok first ->
                when f1 is
                    Ok second -> Ok {first, second}
                    Err NoField -> Err TooShort
            Err NoField -> Err TooShort

    Decode.custom \bytes, fmt -> Decode.decodeWith bytes (Decode.record initialState stepField finalizer) fmt

main =
    when Str.toUtf8 "{\"first\":\"ab\",\"second\":[\"cd\",\"ef\"]}" |> Decode.decodeWith theDecoder Json.fromUtf8 is
        {result, rest: _} ->
            when result is
                Ok { first, second } -> Str.concat first (Str.joinWith second ",")
                Err _ -> "<bad>"
