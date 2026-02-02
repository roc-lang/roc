app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_file : {} -> File
make_file = |{}| {
    { name: "test.txt", content: "hello" }
}

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |_types_list| {
    file = make_file({})
    Ok([file])
}
