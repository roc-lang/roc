app "ingested-file-bytes"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "ingested-file.roc" as ownCode : _, # A type hole can also be used here.
    ]
    provides [main] to pf

main =
    # Due to how ownCode is used, it will be a List U8.
    ownCode
    |> List.map Num.toNat
    |> List.sum
    |> Num.toStr
    |> Stdout.line
