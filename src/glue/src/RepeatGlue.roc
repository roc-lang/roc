app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |_types_list| {
    file : File
    file = { name: "test.txt", content: "hello" }
    files = List.repeat(file, 1)
    Ok(files)
}
