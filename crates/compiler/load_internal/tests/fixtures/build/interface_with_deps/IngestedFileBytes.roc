interface IngestedFileBytes
    exposes [str]

import "IngestedFileBytes.roc" as foo : List U8

str = Str.fromUtf8 foo |> Result.withDefault ""
