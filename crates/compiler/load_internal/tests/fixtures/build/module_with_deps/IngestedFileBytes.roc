module [str]

import "IngestedFileBytes.roc" as foo : List U8

str = Str.from_utf8(foo) |> Result.with_default("")
