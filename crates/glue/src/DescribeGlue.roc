app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List Types -> Result (List File) Str
make_glue = \types ->
    Ok([
        {
            name: "types.txt",
            content: List.map(types, Inspect.to_str) |> Str.join_with("\n"),
        },
    ])
