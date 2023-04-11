app "ingested-file-bytes"
    packages { pf: "cli-platform/main.roc" }
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
