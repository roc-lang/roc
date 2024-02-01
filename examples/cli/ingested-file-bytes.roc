app "ingested-file-bytes"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    provides [main] to pf

import pf.Stdout
import "../../LICENSE" as license : _

main =
    # Due to how license is used, it will be a List U8.
    license
    |> List.map Num.toNat
    |> List.sum
    |> Num.toStr
    |> Stdout.line
