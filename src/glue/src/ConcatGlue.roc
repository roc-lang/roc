app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |_types_list| {
    part1 = "hello"
    part2 = " world"
    content = Str.concat(part1, part2)
    Ok([{ name: "test.txt", content }])
}
