app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
    count = List.len(types_list)
    content = "types count: ${U64.to_str(count)}"
    Ok([{ name: "test.txt", content }])
}
