app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |_types_list| {
    var $content = ""
    for i in [1, 2, 3] {
        $content = Str.concat($content, "line ${U64.to_str(i)}\n")
    }
    Ok([{ name: "test.txt", content: $content }])
}
