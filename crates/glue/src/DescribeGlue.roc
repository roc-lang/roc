app [makeGlue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

makeGlue : List Types -> Result (List File) Str
makeGlue = \types ->
    Ok [
        {
            name: "types.txt",
            content: List.map types Inspect.toStr |> Str.joinWith "\n",
        },
    ]
