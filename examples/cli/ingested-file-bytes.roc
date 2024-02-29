app "ingested-file-bytes"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    provides [main] to pf

import pf.Stdout
import "../../LICENSE" as license : _ # A type hole can also be used here.

main =
    # Due to how license is used, it will be a List U8.
    license
    |> List.map Num.toU64
    |> List.sum
    |> Num.toStr
    |> Stdout.line
