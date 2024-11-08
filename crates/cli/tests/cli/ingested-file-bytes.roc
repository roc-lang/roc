app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br" }

import pf.Stdout
import "test-file.txt" as testFile : _ # the _ is optional

main =
    # Due to the functions we apply on testFile, it will be inferred as a List U8.
    testFile
        |> List.map Num.toU64
        |> List.sum
        |> Num.toStr
        |> Stdout.line!
