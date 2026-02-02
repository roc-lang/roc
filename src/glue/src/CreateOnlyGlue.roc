app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |_types_list| {
    # Create a File but don't use it
    _file = { name: "test.txt", content: "hello" }
    dbg "created file"
    Ok([])
}
