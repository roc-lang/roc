app "ingested-file-bytes"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.0/QOQW08n38nHHrVVkJNiPIjzjvbR3iMjXeFY5w1aT46w.tar.br" }
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
