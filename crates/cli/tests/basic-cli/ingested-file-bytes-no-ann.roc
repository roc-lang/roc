app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br" }

import pf.Stdout
import "test-file.txt" as testFile

main =
    # Due to the functions we apply on testFile, it will be inferred as a List U8.
    testFile
        |> List.map Num.toU64
        |> List.sum
        |> Num.toStr
        |> Stdout.line!
