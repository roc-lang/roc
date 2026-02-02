app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |_types_list| {
    file = { name: "test.txt", content: "hello" }
    files : List(File)
    files = []
    _result = List.append(files, file)
    dbg "after append"
    Ok([])
}
