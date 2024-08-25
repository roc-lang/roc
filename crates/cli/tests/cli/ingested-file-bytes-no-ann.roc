app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.14.0/dC5ceT962N_4jmoyoffVdphJ_4GlW3YMhAPyGPr-nU0.tar.br" }

import pf.Stdout
import "test-file.txt" as testFile

main =
    # Due to the functions we apply on testFile, it will be inferred as a List U8.
    testFile
        |> List.map Num.toU64
        |> List.sum
        |> Num.toStr
        |> Stdout.line!
