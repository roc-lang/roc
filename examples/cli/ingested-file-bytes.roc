app "ingested-file-bytes"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br" }
    imports [
        pf.Stdout,
        "../../LICENSE" as license : _, # A type hole can also be used here.
    ]
    provides [main] to pf

main =
    # Due to how license is used, it will be a List U8.
    license
    |> List.map Num.toU64
    |> List.sum
    |> Num.toStr
    |> Stdout.line
