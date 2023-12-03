interface IngestedFileBytes
    exposes [str]
    imports ["IngestedFileBytes.roc" as foo : List U8]

str = Str.fromUtf8 foo |> Result.withDefault ""
